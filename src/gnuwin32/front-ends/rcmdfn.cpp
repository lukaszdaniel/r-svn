/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2023  R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not,  a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <cstdlib> /* for exit */
#include <cstdio>
#include <Localization.h>
#include <Rembedded.h>
#include <Rinterface.h>
#include <Rversion.h>
#include "console.h"

#ifndef BINDIR
# define BINDIR "bin"
#endif

NORET void R_Suicide(const char *s) /* for call from ../rhome.c */
{
    fprintf(stderr, "FATAL ERROR:%s\n", s);
    std::exit(2);
}


static int pwait(HANDLE p)
{
    DWORD ret;

    WaitForSingleObject(p, INFINITE);
    GetExitCodeProcess(p, &ret);
    return ret;
}

# include <sys/stat.h>

#if !defined(S_IFDIR) && defined(__S_IFDIR)
# define S_IFDIR __S_IFDIR
#endif

static bool isDir(const char *path)
{
    struct stat sb;
    bool isdir = 0;
    if (path[0] && stat(path, &sb) == 0)
    {
        isdir = (sb.st_mode & S_IFDIR) > 0;
    }
    return isdir;
}


void rcmdusage(const char *RCMD)
{
    fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
	    "where 'command' is one of:\n",
	    "  INSTALL  Install add-on packages\n",
	    "  REMOVE   Remove add-on packages\n",
	    "  SHLIB    Make a DLL for use with dynload\n",
	    "  BATCH    Run R in batch mode\n",
	    "  build    Build add-on packages\n",
	    "  check    Check add-on packages\n",
	    "  Rprof    Post process R profiling files\n",
	    "  Rdconv   Convert Rd format to various other formats\n",
	    "  Rdiff    difference R output files\n",
	    "  Rd2pdf   Convert Rd format to PDF\n",
	    "  Rd2txt   Convert Rd format to pretty text\n",
	    "  Stangle   Extract S/R code from vignette\n",
	    "  Sweave    Process vignette documentation\n",
	    "  config   Obtain configuration information about R\n"
	    "  open     Open a file via Windows file associations\n"
	    "  texify   Process a latex file\n"
	    );

    fprintf(stderr, "\n%s%s%s%s",
	    "Use\n  ", RCMD, " command --help\n",
	    "for usage information for each command.\n\n");
}

/* length of argument if quoted to be passed as a second or 
   later to a program conforming to MSVC conventions */
size_t quoted_arg_len(const char *arg)
{
    size_t len = 0;
    size_t nbackslashes = 0;
    for (size_t i = 0; arg[i]; i++) {
	if (arg[i] == '\\')
	    nbackslashes++;
	else if (arg[i] == '"') {
	    /* double backslashes before quote and escape the quote */
	    len += 2 * nbackslashes;
	    nbackslashes = 0;
	    len += 2; /* \" */
	} else {
	    len += nbackslashes;
	    nbackslashes = 0;
	    len ++;
	}
    }
    /* double trailing backslashes due to the following quote */
    len += 2 * nbackslashes;
    len += 2; /* surrounding double quotes */
    return len;
}

/* strcat a quoted version of the argument to dest */
char *quoted_arg_cat(char *dest, const char *arg)
{
    size_t j = strlen(dest);
    size_t nbackslashes = 0;

    dest[j++] = '"';
    for (size_t i = 0; arg[i] ; i++) {
	if (arg[i] == '\\')
	    nbackslashes++;
	else if (arg[i] == '"') {
	    /* double backslashes before quote and escape the quote */
	    for (;nbackslashes; nbackslashes--) {
		dest[j++] = '\\';
		dest[j++] = '\\';
	    }
	    dest[j++] = '\\';
	    dest[j++] = '"';
	} else {
	    for (;nbackslashes; nbackslashes--) dest[j++] = '\\';
	    dest[j++] = arg[i]; 
	}
    }
    /* double trailing backslashes due to the following quote */
    for (;nbackslashes; nbackslashes--) {
	dest[j++] = '\\';
	dest[j++] = '\\';
    }
    dest[j++] = '"';
    dest[j] = '\0';
    return dest;
}

static BOOL WINAPI CtrlHandler(DWORD type)
{
    /* ignore Ctrl-C; R handles Ctrl+Break the same way (see psignal) */
    return (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT);
}

#define PROCESS_CMD(ARG)	if (cmdarg + 1 < argc) {\
	    for (int i = cmdarg + 1; i < argc; i++) {\
		strcat(cmd, ARG);\
		if (strlen(cmd) + quoted_arg_len(argv[i]) > 9900) {\
		    fprintf(stderr, "command line too long\n");\
		    freeRHOME(RHome);\
		    return(27);\
		}\
		quoted_arg_cat(cmd, argv[i]);\
	    }\
	    /* the outermost double quotes are needed for cmd.exe */\
	    strcat(cmd,"\"");\
	}\
	freeRHOME(RHome);\
	return(system(cmd))


extern int process_Renviron(const char *filename);
#define CMD_LEN 10000
int rcmdfn(int cmdarg, int argc, char **argv)
{
    /* tasks:
       find R_HOME, set as env variable (with / as separator)
       set R_ARCH
       set PATH to include R_HOME\bin
       set TMPDIR if unset
       set HOME if unset
       set R_CMD (depends on how this was launched), R_VERSION
       read R_HOME\etc\Rcmd_environ
       launch %R_HOME%\bin\$*
    */
    int iused;
    char *p, cmd[CMD_LEN];
    char RCMD[] = "R CMD";
    int len = strlen(argv[0]);
    char *env_path;
    int timing = 1;
    int dirstrip = 2;
#ifdef R_ARCH
    if (strlen(R_ARCH) > 0)
	dirstrip++;
#endif 
    char *RHome = getRHOME(dirstrip);

    if(!RHome)
        R_Suicide(_("Invalid R_HOME"));

    if(!strncmp(argv[0]+len-4, "Rcmd", 4) ||
       !strncmp(argv[0]+len-4, "rcmd", 4) ||
       !strncmp(argv[0]+len-8, "Rcmd.exe", 8) ||
       !strncmp(argv[0]+len-8, "rcmd.exe", 8))
	strcpy(RCMD, "Rcmd");


    if (argc <= cmdarg) {
	fprintf(stderr, "%s%s%s", "Usage: ", RCMD, " command args\n\n");
	rcmdusage(RCMD);
	return(0);
    }

    if (argc == cmdarg+1 &&
	(!strcmp(argv[cmdarg], "--help") || !strcmp(argv[cmdarg], "-h"))
	) {
	/* need to cover Rcmd --help, R CMD --help and R --help,
	   as well as -h versions.
	 */
	if(cmdarg >= 2 || (cmdarg == 1 && !strcmp(RCMD, "Rcmd"))) {
	    fprintf(stderr, "%s%s%s", "Usage: ", RCMD, " command args\n\n");
	    rcmdusage(RCMD);
	    return(0);
	}
	/* R --help */
	snprintf(cmd, CMD_LEN, "\"%s/%s/Rterm.exe\" --help", RHome, BINDIR);
	system(cmd);
	fprintf(stderr, "%s", "\n\nOr: R CMD command args\n\n");
	rcmdusage(RCMD);
	return(0);
    }

    if (cmdarg == 0) {
	/* use of R.exe without CMD, -h, --help */
	if (argc > 1 && !strcmp(argv[1], "RHOME")) {
	    /* An historical special case */
	    fprintf(stdout, "%s", RHome);
	    return(0);
	}
	snprintf(cmd, CMD_LEN, "\"\"%s/%s/Rterm.exe\"", RHome, BINDIR);
	/* R.exe should ignore Ctrl-C, and let Rterm.exe handle it */
        /*   don't SetConsoleCtrlHandler(NULL, TRUE) to preserve
             the current setting of the inheritable attribute */
        SetConsoleCtrlHandler(CtrlHandler, TRUE);
	PROCESS_CMD(" ");
    }

    /* From here on down, this was called as Rcmd or R CMD */

    R_putenv_path_cpy("R_HOME", RHome, 1);

    /* We follow Unix-alikes as from R 2.12.0 in setting environment
       variables in Rcmd BATCH.

       NB: Rcmd_environ uses R_HOME.
    */
    env_path = (char*)malloc(strlen(RHome) + strlen("/etc/Rcmd_environ") + 1);
    if (!env_path)
	R_Suicide(_("Allocation error"));
    strcpy(env_path, RHome); strcat(env_path, "/etc/Rcmd_environ");
    process_Renviron(env_path);
    free(env_path);

    if (!strcmp(argv[cmdarg], "BATCH")) {
	/* ----- handle Rcmd BATCH  ---- */
	char *infile, *outfile, *p, cmd_extra[CMD_LEN];
	int allocated_outfile = 0;
	DWORD ret;
	SECURITY_ATTRIBUTES sa;
	PROCESS_INFORMATION pi;
	STARTUPINFO si;
	HANDLE hOUT = INVALID_HANDLE_VALUE;

	/* process the command line */
	cmd_extra[0] = '\0';
	if((p = getenv("R_BATCH_OPTIONS")) && strlen(p)) {
	    if(1+strlen(p) >= CMD_LEN) {
		fprintf(stderr, "command line too long\n");
		return(27);
	    }
	    strcat(cmd_extra, " ");
	    strcat(cmd_extra, p);
	}

	iused = cmdarg;
	for (int i = cmdarg + 1; i < argc; i++) {
	    if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
		fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
"Usage: ", RCMD, " BATCH [options] infile [outfile]\n\n",
"Run R non-interactively with input from infile and place output (stdout\n",
"and stderr) to another file.  If not given, the name of the output file\n",
"is the one of the input file, with a possible '.R' extension stripped,\n",
"and '.Rout' appended.\n\n",
"Options:\n"
"  -h, --help		print short help message and exit\n",
"  -v, --version		print version info and exit\n",
"  --no-timing		do not report the timings\n",
"  --			end processing of options\n\n",
"Further arguments starting with a '-' are considered as options as long\n",
"as '--' was not encountered, and are passed on to the R process, which\n",
"by default is started with '--restore --save'.\n\n",
"Report bugs at <https://bugs.R-project.org>.");
		return(0);
	    }
	    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--version")) {
		fprintf(stderr, "R batch front end: %s.%s (r%d)\n\n%s%s%s%s%s\n",
R_MAJOR, R_MINOR, R_SVN_REVISION,
"Copyright (C) 1997-", R_YEAR, " The R Core Team.\n",
"This is free software; see the GNU General Public Licence version 2\n",
"or later for copying conditions.  There is NO warranty.");
		return(0);
	    }
	    if (!strcmp(argv[i], "--no-timing")) {
		timing = 0;
		iused = i;
		continue;
	    }

	    if (!strcmp(argv[i], "--")) {
		iused = i;
		break;
	    }
	    if (argv[i][0] == '-') {
		if (strlen(cmd_extra) + strlen(argv[i]) > 9900) {
		    fprintf(stderr, "command line too long\n");
		    return(27);
		}
		strcat(cmd_extra, " ");
		strcat(cmd_extra, argv[i]);
		iused = i;
	    } else break;
	}
	if (iused+1 < argc)
	    infile = argv[iused+1];
	else {
	    fprintf(stderr, "no input file\n");
	    return(1);
	}
	if (iused+2 < argc)
	    outfile = argv[iused+2];
	else {
	    int len = strlen(infile);
	    outfile = (char *)malloc(len + strlen(".Rout") + 1);
	    if (!outfile)
		R_Suicide(_("Allocation error"));
	    allocated_outfile = 1;
	    strcpy(outfile, infile);
	    if (!strcmp(outfile+len-2, ".R")) strcat(outfile, "out");
	    else strcat(outfile, ".Rout");
	}

	/* Unix has --restore --save --no-readline */
	/* cmd is used with CreateProcess, hence no outermost double quotes */
	snprintf(cmd, CMD_LEN, "\"%s/%s/Rterm.exe\" -f \"%s\" --restore --save",
		 RHome, BINDIR, infile);
	if(strlen(cmd) + strlen(cmd_extra) >= CMD_LEN) {
	    fprintf(stderr, "command line too long\n");
	    return(27);
	}
	strcat(cmd, cmd_extra);

	/* to get .Last.sys run: see profile/Common.R */
	if(timing) putenv("R_BATCH=1234");

	/* fprintf(stderr, "%s->%s\n", infile, outfile);
	   fprintf(stderr, "%s\n", cmd); */

	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;

	hOUT = CreateFile(outfile, GENERIC_WRITE, FILE_SHARE_READ,
			  &sa, CREATE_ALWAYS, 0, NULL);
	if (hOUT == INVALID_HANDLE_VALUE) {
	    fprintf(stderr, "unable to open output file\n");
	    return(2);
	}
	SetStdHandle(STD_OUTPUT_HANDLE, hOUT);
	SetStdHandle(STD_ERROR_HANDLE, hOUT);
	si.cb = sizeof(si);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_SHOWDEFAULT;
	ret = CreateProcess(0, cmd, &sa, &sa, TRUE, 0, NULL, NULL, &si, &pi);
	CloseHandle(hOUT);
	if (allocated_outfile)
	    free(outfile);
	if (!ret) {
	    fprintf(stderr, "unable to run Rterm.exe\n");
	    return(3);
	}
	CloseHandle(pi.hThread);
	return(pwait(pi.hProcess));
	/* ------- end of BATCH -------- */
    }

    /* Now Rcmd <cmd> or R CMD <cmd>: most commands are nowadays
     * handled internally on Windows
     */

    /* Not sure that we still need these set -- they are Windows-only */
    char *Rversion = (char *)malloc(25);
    if (!Rversion)
	R_Suicide(_("Allocation error"));
    snprintf(Rversion, 25, "R_VERSION=%s.%s", R_MAJOR, R_MINOR);
    putenv(Rversion);
    /* no free here: storage remains in use */

    putenv("R_CMD=R CMD");

    const char *oldpath = getenv("PATH");
    const char *pformat = "PATH=%s\\%s;%s";
    size_t needed = snprintf(NULL, 0, pformat, RHome, BINDIR, oldpath) + 1;
    char *newpath = (char *)malloc(needed);
    if (!newpath) {fprintf(stderr, "PATH too long\n"); return(4);}
    snprintf(newpath, needed, pformat, RHome, BINDIR, oldpath);
    putenv(newpath);
    /* no free here: storage remains in use */

    char *Rarch = (char *) malloc(30);
    if (!Rarch)
	R_Suicide(_("Allocation error"));
    strcpy(Rarch, "R_ARCH=");
#ifdef R_ARCH
    if (strlen(R_ARCH) > 0) {
	strcat(Rarch, "/");
	strcat(Rarch, R_ARCH);
    }
#endif
    putenv(Rarch);
    /* no free here: storage remains in use */

    R_putenv_path_cpy("BINDIR", BINDIR, 0);

    char *RUser = getRUser();

    if ((p = getenv("TMPDIR")) && isDir(p)) {
	/* TMPDIR is already set */
    } else {
	if ((p = getenv("TEMP")) && isDir(p))
	    R_putenv_path_cpy("TMPDIR", p, 0);
	else if ((p = getenv("TMP")) && isDir(p))
	    R_putenv_path_cpy("TMPDIR", p, 0);
	else
	    R_putenv_path_cpy("TMPDIR", RUser, 0);
    }

    if (!getenv("HOME"))
	R_putenv_path_cpy("HOME", RUser, 0);

    freeRUser(RUser);

    if (!strcmp(argv[cmdarg], "INSTALL")) {
	/* Unix has --no-restore except for MM's undocumented --use-vanilla */
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.install_packages() R_DEFAULT_PACKAGES= LC_COLLATE=C --no-restore --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "REMOVE")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -f \"%s/share/R/REMOVE.R\" R_DEFAULT_PACKAGES=NULL --no-restore --no-echo --args",
		 RHome, BINDIR, RHome);
	PROCESS_CMD(" ");
    } else if (!strcmp(argv[cmdarg], "build")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.build_packages() R_DEFAULT_PACKAGES= LC_COLLATE=C --no-restore --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "check")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.check_packages() R_DEFAULT_PACKAGES= LC_COLLATE=C --no-restore --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Rprof")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.Rprof() R_DEFAULT_PACKAGES=utils LC_COLLATE=C --vanilla --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
   } else if (!strcmp(argv[cmdarg], "texify")) {
	if (argc < cmdarg+2) {
	    fprintf(stderr, "\nUsage: %s texify [options] filename\n", RCMD);
	    return(1);
	}
	snprintf(cmd, CMD_LEN,
		 "\"texify.exe -I \"%s/share/texmf/tex/latex\" -I \"%s/share/texmf/bibtex/bst\"", RHome, RHome);
	PROCESS_CMD(" ");
    } else if (!strcmp(argv[cmdarg], "SHLIB")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.SHLIB() R_DEFAULT_PACKAGES=NULL --no-restore --no-echo --no-site-file --no-init-file --args",
		 RHome, BINDIR);
	PROCESS_CMD(" ");
    } else if (!strcmp(argv[cmdarg], "Rdiff")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.Rdiff() R_DEFAULT_PACKAGES=NULL --vanilla --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Rdconv")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.Rdconv() R_DEFAULT_PACKAGES= LC_COLLATE=C --vanilla --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Rd2txt")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::.Rdconv() R_DEFAULT_PACKAGES= LC_COLLATE=C --vanilla --no-echo --args nextArg-tnextArgtxt",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Rd2pdf")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" -e tools:::..Rd2pdf() R_DEFAULT_PACKAGES= LC_ALL=C --vanilla --no-echo --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Sweave")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" --no-restore --no-echo -e utils:::.Sweave() --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else if (!strcmp(argv[cmdarg], "Stangle")) {
	snprintf(cmd, CMD_LEN,
		 "\"\"%s/%s/Rterm.exe\" --vanilla --no-echo -e utils:::.Stangle() --args ",
		 RHome, BINDIR);
	PROCESS_CMD("nextArg");
    } else {
	/* not one of those handled internally */
	p = argv[cmdarg];
	if (!strcmp(p, "config"))
	    snprintf(cmd, CMD_LEN, "\"sh \"%s/bin/config.sh\"", RHome);
	else if (!strcmp(p, "open"))
	    snprintf(cmd, CMD_LEN, "\"\"%s/%s/open.exe\"", RHome, BINDIR);
	else {
	    /* RHOME/BINDIR is first in the path, so looks there first */
	    if (!strcmp(".sh", p + strlen(p) - 3)) strcpy(cmd, "\"sh ");
	    else if (!strcmp(".pl", p + strlen(p) - 3)) strcpy(cmd, "\"perl ");
	    else strcpy(cmd, "\"");
	    strcat(cmd, p);
	}
	PROCESS_CMD(" ");
    }
    /* not reachable */
}

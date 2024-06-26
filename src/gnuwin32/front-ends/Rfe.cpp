/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2010--2023  R Core Team
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
#include <windows.h> /* for SetConsoleCtrlHandler */
#include <cstdlib>  /* for exit */
#include <cstring>
#include <cstdio>
#include "console.h"

static void Usage(const char *RCMD, const char *arch)
{
    fprintf(stderr, "%s %s %s", "Usage:", RCMD, "[command args]\n\n");
    fprintf(stderr, "%s%s%s",
	    "where 'command args' can be\n\n",
	    "  --arch n   for n=i386, x64, 32 or 64\n",
	    "  any other arguments listed by ");
    fprintf(stderr, "%s --arch %s --help\n", RCMD, arch);
}

static BOOL WINAPI CtrlHandler(DWORD type)
{
    /* ignore Ctrl-C; R handles Ctrl+Break the same way (see psignal) */
    return (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT);
}

#define CMD_LEN 10000
int main(int argc, char **argv)
{
    int cmdarg = 1;
    int interactive = 0;
    char arch[10] = R_ARCH, cmd[CMD_LEN], *p;

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
	Usage(argv[0], arch);
	std::exit(0);
    }
    
    if (argc > 1 && strcmp(argv[1], "--arch") == 0) {
	cmdarg = 3;
	if(argc < 3) {
	    Usage(argv[0], arch);
	    std::exit(0);
	}
	strncpy(arch, argv[2], 10); arch[9] = '\0';
	if(strcmp(arch, "32") == 0) strcpy(arch, "i386");
	if(strcmp(arch, "64") == 0) strcpy(arch, "x64");
	if(strcmp(arch, "i386") && strcmp(arch, "x64")) {
	    fprintf(stderr, "valid values for --arch are i386, x64, 32, 64\n");
	    std::exit(1);
	}
    } else if ((p = getenv("R_ARCH"))) {
	strncpy(arch, p+1, 10 - 1); /* skip leading slash */
	arch[10 - 1] = '\0';
    }
 
    char *rhome2 = getRHOME(2);
    if (!rhome2) {
	fprintf(stderr, "Invalid R_HOME\n");
	std::exit(1);
    }
    if (stricmp(argv[0] + strlen(argv[0]) - 11, "Rscript.exe") == 0
	|| stricmp(argv[0] + strlen(argv[0]) - 7, "Rscript") == 0)
	snprintf(cmd, CMD_LEN, "\"\"%s\\bin\\%s\\Rscript.exe\"", rhome2, arch);
    else {
    	snprintf(cmd, CMD_LEN, "\"\"%s\\bin\\%s\\R.exe\"", rhome2, arch);
	interactive = 1;
    }
    freeRHOME(rhome2);

    for (int i = cmdarg; i < argc; i++) {
	if (interactive && !strcmp(argv[i], "CMD"))
	    interactive = 0;
	if (strlen(cmd) + quoted_arg_len(argv[i]) + 2 > CMD_LEN) {
	    fprintf(stderr, "command line too long\n");
	    std::exit(27);
	}
	strcat(cmd, " ");
	quoted_arg_cat(cmd, argv[i]);
    }
    /* the outermost double quotes are needed for cmd.exe */
    if (strlen(cmd) + 1 > CMD_LEN) {
	fprintf(stderr, "command line too long\n");
	std::exit(27);
    }
    strcat(cmd, "\"");

    if (interactive)
	/* Ignore Ctrl-C so that Rterm.exe can handle it */
	/*   don't SetConsoleCtrlHandler(NULL, TRUE) to preserve
	     the current setting of the inheritable attribute */
	SetConsoleCtrlHandler(CtrlHandler, TRUE);
    
    std::exit(system(cmd));
 }

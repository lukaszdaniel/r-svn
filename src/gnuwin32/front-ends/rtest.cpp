/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2023  R Core Team
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

#define Win32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <cstdio>
#include <Rversion.h>
#define LibExtern __declspec(dllimport) extern
#include <Rembedded.h>
#include <R_ext/RStartup.h>
#include <CXXR/RObject.hpp> // for UserBreak
/* for askok and askyesnocancel */
#include <graphapp.h>

/* for signal-handling code */
#include <psignal.h>

/* simple input, simple output */

/* This version blocks all events: a real one needs to call ProcessEvents
   frequently. See rterm.c and ../system.c for one approach using
   a separate thread for input.
*/
int myReadConsole(const char *prompt, unsigned char *buf, int len,
                  int addtohistory)
{
    fputs(prompt, stdout);
    fflush(stdout);
    if(fgets((char *)buf, len, stdin)) return 1;
    else return 0;
}

void myWriteConsoleEx(const char *buf, int len, otype_t otype)
{
    if (otype == 1 /* INFORMATION_ */)
    {
        printf("\033[92m%s\033[0m", buf);
    }
    else if (otype == 2 /* WARNING_ */)
    {
        printf("\033[93m%s\033[0m", buf);
    }
    else if (otype == 3 /* ERROR_ */)
    {
        printf("\033[91m%s\033[0m", buf);
    }
    else /* Normal output (sent to stdout) */
    {
        printf("%s", buf);
    }
}

void myCallBack(void)
{
    /* called during i/o, eval, graphics in ProcessEvents */
}

void myBusy(int which)
{
    /* set a busy cursor ... if which = 1, unset if which = 0 */
}

static void my_onintr(int sig)
{
    UserBreak = TRUE;
}

int main(int argc, char **argv)
{
    structRstart rp;
    Rstart Rp = &rp;
    char Rversion[25], *RHome, *RUser;

    snprintf(Rversion, 25, "%s.%s", R_MAJOR, R_MINOR);
    if (strcmp(getDLLVersion(), Rversion) != 0) {
        fprintf(stderr, "Error: R.DLL version does not match\n");
        std::exit(1);
    }

    R_setStartTime();
    R_DefParamsEx(Rp, RSTART_VERSION);
    if((RHome = get_R_HOME()) == NULL) {
	fprintf(stderr, 
		"R_HOME must be set in the environment or Registry\n");
	std::exit(1);
    }
    Rp->rhome = RHome;
    RUser = getRUser();
    Rp->home = RUser;
    Rp->CharacterMode = LinkDLL;
    Rp->EmitEmbeddedUTF8 = FALSE;
    Rp->ReadConsole = myReadConsole;
    Rp->WriteConsole = NULL; /* for illustration purposes we use more flexible WriteConsoleEx */
    Rp->WriteConsoleEx = myWriteConsoleEx;
    Rp->CallBack = myCallBack;
    Rp->ShowMessage = askok;
    Rp->YesNoCancel = askyesnocancel;
    Rp->Busy = myBusy;

    Rp->R_Quiet = TRUE;
    Rp->R_Interactive = FALSE;
    Rp->RestoreAction = SA_RESTORE;
    Rp->SaveAction = SA_NOSAVE;
    R_SetParams(Rp);
    freeRUser(RUser);
    free_R_HOME(RHome);
    R_set_command_line_arguments(argc, argv);

    FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));

    signal(SIGBREAK, my_onintr);
    GA_initapp(0, 0);
    readconsolecfg();
    setup_Rmainloop();
#ifdef SIMPLE_CASE
    run_Rmainloop();
#else
    R_ReplDLLinit();
    while(R_ReplDLLdo1() > 0) {
/* add user actions here if desired */
    }
/* only get here on EOF (not q()) */
#endif
    Rf_endEmbeddedR(0);
    return 0;
}

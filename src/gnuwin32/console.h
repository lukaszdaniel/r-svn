/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file console.h
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004-8      The R Foundation
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef CONSOLE_H
#define CONSOLE_H

#ifndef __cplusplus
#include <stdbool.h>
#endif
#include "graphapp/ga.h"

typedef window console;
typedef window pager;
typedef window dataeditor;
typedef window editor;

char *getRHOME(int);
void freeRHOME(char *);
void R_putenv_path_cpy(const char *varname, const char *value, int fixslash);
size_t quoted_arg_len(const char *arg); /* in rcmdfn.c */
char *quoted_arg_cat(char *dest, const char *arg);
int rcmdfn(int cmdarg, int argc, char **argv); /* in rcmdfn.c */
int cmdlineoptions(int ac, char **av);
char *getRHOMElong(int m); /* in ../rhome.c */
void freeRHOMElong(char *s);
void saveConsoleTitle(void);
void R_gl_tab_set(void);

void setconsoleoptions(const char *fnname,int fnsty, int fnpoints,
		  int rows, int cols, int consx, int consy,
		  rgb *nguiColors,
		  int pgr, int pgc, int multiplewindows, int widthonresize,
		  int bufbytes, int buflines, int buffered, int cursor_blink);
pager newpager(const char *title, const char *filename, int enc,
	       const char *header, bool unlinkonexit);
console newconsole(const char *name, int flags);
int  consolereads(console c, const char *prompt, char *buf, int len,
		  int addtohistory);
int  consolewrites(console c, const char *s);
int  consolecancopy(console c);
int  consolecanpaste(console c);
void consolecopy(console c);
void consolepaste(console c);
void consolepastecmds(console c);
void consoleselectall(console c);
void consolecmd(console c, const char *cmd);
void consolenewline(console c);
void consolehelp(void);
void consolesetbrk(console c, actionfn, char ch, char mod);
void consoletogglelazy(console c);
int  consolegetlazy(console c);
void consoleflush(console c);
void consoleprint(console c);
void consolesavefile(console c, int pager);
void drawconsole(control c, rect r);
void consoleclear(control c);

extern bool setWidthOnResize;
extern int consolebuffered;
int consolecols(console c);
void pagerbclose(control m);

#endif // CONSOLE_H

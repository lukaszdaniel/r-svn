/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2022   The R Core Team
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/* <UTF8> char here is handled as a whole string, but note that
   fprintf is used */

/** @file edit.cpp
 *
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cstdio>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Localization.h>
#include <Rembedded.h>
#include <Defn.h>
#include <Internal.h>
#include <Print.h>
#include <Fileio.h>
#include <Parse.h>

#ifdef Win32
# include "run.h"
int Rgui_Edit(const char *filename, int enc, const char *title, int modal);
#endif

#ifdef Unix
#define R_INTERFACE_PTRS 1
#include <Rinterface.h> /* for editor ptr */
#endif


#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* for unlink() */
#endif

using namespace R;

/*
 * ed, vi etc have 3 parameters. the data, a file and an editor
 *
 * If `file' is specified then the given file is used (and not removed on
 * exit). If `file' is not specified then a temporary file is used; since
 * only one temporary file is used for an entire session previous
 * editing is lost. That file is removed at the end of the R session.
 *
 * If `data' is specified then it is passed out to be edited; if `data' is not
 * specified then either `file' (if specified) or the temporary file is used
 * (thus errors can be re-edited by calling edit a second time with no
 * arguments).
 *
 * If the editor is specified then the specified editor is invoked if
 * possible and an error message reported otherwise
 */

static char *DefaultFileName;
static int  EdFileUsed = 0;

attribute_hidden void R::InitEd(void)
{
#ifdef Win32
    DefaultFileName = R_tmpnam2("Redit", R_TempDir, ".R");
#else
    DefaultFileName = R_tmpnam2(NULL, R_TempDir, ".R");
#endif
}

void CleanEd(void)
{
    if(EdFileUsed) unlink(DefaultFileName);
}

SEXP do_edit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int   i, rc;
    ParseStatus status;
    SEXP  x, fn, envir, ed, src, srcfile, Rfn;
    char *filename, *editcmd;
    const char *cmd;
    FILE *fp;
#ifdef Win32
    SEXP ti;
    char *title;
#endif

    checkArity(op, args);

    CXXR::RAllocStack::Scope rscope;

    x = CAR(args); args = CDR(args);
    if (TYPEOF(x) == CLOSXP) envir = CLOENV(x);
    else envir = R_NilValue;
    PROTECT(envir);

    fn = CAR(args); args = CDR(args);
    if (!isString(fn))
	error(_("invalid argument to '%s'"), "edit()");

    if (LENGTH(STRING_ELT(fn, 0)) > 0) {
	const char *ss = translateCharFP(STRING_ELT(fn, 0));
	filename = R_alloc(strlen(ss), sizeof(char));
	strcpy(filename, ss);
    }
    else filename = DefaultFileName;

    if (x != R_NilValue) {
	if((fp=R_fopen(R_ExpandFileName(filename), "w")) == NULL)
	    errorcall(call, "%s", _("unable to open file"));
	if (LENGTH(STRING_ELT(fn, 0)) == 0) EdFileUsed++;
	PROTECT(src = deparse1(x, FALSE, FORSOURCING)); /* deparse for sourcing, not for display */
	for (i = 0; i < LENGTH(src); i++)
	    fprintf(fp, "%s\n", translateChar(STRING_ELT(src, i)));
	UNPROTECT(1); /* src */
	fclose(fp);
    }
#ifdef Win32
    ti = CAR(args);
#endif
    args = CDR(args);
    ed = CAR(args);
    if (!isString(ed)) errorcall(call, "%s", _("argument 'editor' type not valid"));
    cmd = translateCharFP(STRING_ELT(ed, 0));
    if (strlen(cmd) == 0) errorcall(call, "%s", _("argument 'editor' is not set"));
    size_t sz = strlen(cmd) + strlen(filename) + 6;
    editcmd = R_alloc(sz, sizeof(char));
#ifdef Win32
    if (streql(cmd,"internal")) {
	if (!isString(ti))
	    error("%s", _("'title' must be a string"));
	if (LENGTH(STRING_ELT(ti, 0)) > 0) {
	    title = R_alloc(strlen(CHAR(STRING_ELT(ti, 0)))+1, sizeof(char));
	    strcpy(title, CHAR(STRING_ELT(ti, 0)));
	} else {
	    title = R_alloc(strlen(filename)+1, sizeof(char));
	    strcpy(title, filename);
	}
	Rgui_Edit(filename, CE_NATIVE, title, 1);
    }
    else {
	/* Quote path if not quoted */
	if(cmd[0] != '"')
	    snprintf(editcmd, sz, "\"%s\" \"%s\"", cmd, filename);
	else
	    snprintf(editcmd, sz, "%s \"%s\"", cmd, filename);
	rc = runcmd(editcmd, CE_NATIVE, 1, 1, NULL, NULL, NULL);
	if (rc == NOLAUNCH)
	    errorcall(call, _("unable to run editor '%s'"), cmd);
	if (rc != 0)
	    warningcall(call, "%s", _("editor ran but returned error status"));
    }
#else
    if (ptr_R_EditFile)
	rc = ptr_R_EditFile(filename);
    else {
	snprintf(editcmd, sz, "'%s' '%s'", cmd, filename); // allow for spaces
	rc = R_system(editcmd);
    }
    if (rc != 0)
	errorcall(call, _("problem with running editor %s"), cmd);
#endif

    if (asLogical(GetOption1(install("keep.source")))) {
	PROTECT(Rfn = findFun(install("readLines"), R_BaseEnv));
	PROTECT(src = lang2(Rfn, ScalarString(mkChar(R_ExpandFileName(filename)))));
	PROTECT(src = eval(src, R_BaseEnv));
	PROTECT(Rfn = findFun(install("srcfilecopy"), R_BaseEnv));
	PROTECT(srcfile = lang3(Rfn, ScalarString(mkChar("<tmp>")), src));
	srcfile = eval(srcfile, R_BaseEnv);
	UNPROTECT(5);
    } else
	srcfile = R_NilValue;
    PROTECT(srcfile);

    /* <FIXME> setup a context to close the file, and parse and eval
       line by line */
    if((fp = R_fopen(R_ExpandFileName(filename), "r")) == NULL)
	errorcall(call, "%s", _("unable to open file to read"));

    x = PROTECT(R_ParseFile(fp, -1, &status, srcfile));
    fclose(fp);

    if (status != PARSE_OK)
	errorcall(call,
		  _("%s occurred on line %d\n use a command like\n x <- edit()\n to recover"), R_ParseErrorMsg, R_ParseError);
    R_ResetConsole();
    {   /* can't just eval(x) here */
	int j, n;
	SEXP tmp = R_NilValue;

	n = LENGTH(x);
	for (j = 0 ; j < n ; j++)
	    tmp = eval(XVECTOR_ELT(x, j), R_GlobalEnv);
	x = tmp;
    }
    if (TYPEOF(x) == CLOSXP && envir != R_NilValue)
	SET_CLOENV(x, envir);
    UNPROTECT(3);

    return x;
}

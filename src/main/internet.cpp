/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2021   The R Core Team.
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

/** @file internet.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <Localization.h>
#include <Rdynpriv.h>
#include <Defn.h>
#include <Internal.h>

#include <Rconnections.h>
#include <Rmodules/Rinternet.h>

using namespace R;
using namespace CXXR;

static R_InternetRoutines routines, *ptr = &routines;


/*
SEXP Rdownload(SEXP args);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, int serverfd, char *mode, int timeout, int options);
Rconnection R_newservsock(int port);

int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)

int extR_HTTPDCreate(const char *ip, int port);
void extR_HTTPDStop(void);

and more
 */

static int s_initialized = 0;

R_InternetRoutines *R_setInternetRoutines(R_InternetRoutines *routines)
{
    R_InternetRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return tmp;
}

static void internet_Init(void)
{
    int res;
    res = R_moduleCdynload("internet", 1, 1);
    s_initialized = -1;
    if (!res) return;
    if (!ptr->download)
	error("%s", _("internet routines cannot be accessed in module"));
    s_initialized = 1;
    return;
}

SEXP Rdownload(SEXP args)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->download)(args);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

// As from R 4.2.0 this is only used on Windows
attribute_hidden Rconnection R_newurl(const char *description, const char * const mode, SEXP headers, int type)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->newurl)(description, mode, headers, type);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

attribute_hidden Rconnection R_newsock(const char *host, int port, int server, int serverfd,
          const char * const mode, int timeout, int options)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->newsock)(host, port, server, serverfd, mode, timeout, options);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

attribute_hidden Rconnection R_newservsock(int port)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->newservsock)(port);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

int extR_HTTPDCreate(const char *ip, int port)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->HTTPDCreate)(ip, port);
    else
	error("%s", _("internet routines cannot be loaded"));
    return -1;
}

void extR_HTTPDStop(void)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->HTTPDStop)();
    else
	error("%s", _("internet routines cannot be loaded"));
}


SEXP Rsockconnect(SEXP sport, SEXP shost)
{
    if (length(sport) != 1) error(_("invalid '%s' argument"), "socket");
    int port = asInteger(sport);
    char *host[1];
    host[0] = (char *) translateCharFP(STRING_ELT(shost, 0));
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->sockconnect)(&port, host);
    else
	error("%s", _("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

SEXP Rsockread(SEXP ssock, SEXP smaxlen)
{
    if (length(ssock) != 1) error(_("invalid '%s' argument"), "socket");
    int sock = asInteger(ssock);
    int maxlen = asInteger(smaxlen);
    if (maxlen < 0) /* also catches NA_INTEGER */
	error("%s", _("maxlen must be non-negative"));
    GCStackRoot<> rbuf;
    rbuf = allocVector(RAWSXP, maxlen + 1);
    char *buf = (char *) RAW(rbuf), *abuf[1];
    abuf[0] = buf;
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->sockread)(&sock, abuf, &maxlen);
    else
	error("%s", _("socket routines cannot be loaded"));
    if (maxlen < 0) // presumably -1, error from recv
	error("%s", _("Error reading data in Rsockread"));
    GCStackRoot<> ans;
    ans = allocVector(STRSXP, 1);
    SET_STRING_ELT(ans, 0, mkCharLen(buf, maxlen));

    return ans;
}

SEXP Rsockclose(SEXP ssock)
{
    if (length(ssock) != 1) error(_("invalid '%s' argument"), "socket");
    int sock = asInteger(ssock);
    if (sock <= 0) error("%s", _("attempt to close invalid socket"));
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->sockclose)(&sock);
    else
	error("%s", _("socket routines cannot be loaded"));
    return ScalarLogical(sock);
}

SEXP Rsockopen(SEXP sport)
{
    if (length(sport) != 1) error(_("invalid '%s' argument"), "port");
    int port = asInteger(sport);
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->sockopen)(&port);
    else
	error("%s", _("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

SEXP Rsocklisten(SEXP ssock)
{
    if (length(ssock) != 1) error(_("invalid '%s' argument"), "socket");
    int sock = asInteger(ssock), len = 256;
    char buf[257], *abuf[1];
    abuf[0] = buf;
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->socklisten)(&sock, abuf, &len);
    else
	error("%s", _("socket routines cannot be loaded"));
    GCStackRoot<> ans, host;
    ans = ScalarInteger(sock); // The socket being listened on
    host = allocVector(STRSXP, 1);
    SET_STRING_ELT(host, 0, mkChar(buf));
    setAttrib(ans, install("host"), host);

    return ans;
}

SEXP Rsockwrite(SEXP ssock, SEXP sstring)
{
    if (length(ssock) != 1) error(_("invalid '%s' argument"), "socket");
    int sock = asInteger(ssock), start = 0, end, len;
    char *buf = (char *) translateCharFP(STRING_ELT(sstring, 0)), *abuf[1];
    end = len = (int) strlen(buf);
    abuf[0] = buf;
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	(*ptr->sockwrite)(&sock, abuf, &start, &end, &len);
    else
	error("%s", _("socket routines cannot be loaded"));
    return ScalarInteger(len);
}


attribute_hidden
int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->sockselect)(nsock, insockfd, ready, write, timeout);
    else {
	error("%s", _("socket routines cannot be loaded"));
	return 0;
    }
}

attribute_hidden SEXP do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->curlVersion)(call, op, args, rho);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden SEXP do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->curlGetHeaders)(call, op, args, rho);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden SEXP do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->curlDownload)(call, op, args, rho);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden Rconnection R_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type)
{
    if (!s_initialized) internet_Init();
    if (s_initialized > 0)
	return (*ptr->newcurlurl)(description, mode, headers, type);
    else {
	error("%s", _("internet routines cannot be loaded"));
	return (Rconnection)NULL;
    }
    return (Rconnection)NULL; /* -Wall in gcc, but Solaris compiler complains */
}


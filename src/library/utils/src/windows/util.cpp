/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file util.c
 *  Copyright (C) 2005--2024  The R Core Team
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004	      The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <memory>
#include <windows.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/String.hpp>
#include <Defn.h>

#include "../localization.h"
#undef TRUE
#undef FALSE

using namespace R;
using namespace CXXR;

/* FIXME:
   This should include utils.h to force consistency.
*/

typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

// keep in step with src/gnuwin32/extra.c
SEXP winver(void)
{
    char ver[256];
    OSVERSIONINFOEX osvi;

    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    if(!GetVersionEx((OSVERSIONINFO *)&osvi))
	error("%s", _("unsupported version of Windows"));

    /* see https://learn.microsoft.com/en-us/windows/win32/sysinfo/getting-the-system-version?redirectedfrom=MSDN
       for ways to get more info.
       Pre-NT versions are all 4.x, so no need to separate test.
       See also https://learn.microsoft.com/en-us/windows/win32/sysinfo/operating-system-version?redirectedfrom=MSDN
       https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833%28v=vs.85%29.aspx
       for version number naming.
    */
    if(osvi.dwMajorVersion >= 5) {
	const char *desc = "";
	const char *type="";
	SYSTEM_INFO si;
	// future-proof
	snprintf(ver, 256, "%d.%d",
		 (int) osvi.dwMajorVersion, (int) osvi.dwMinorVersion);
	if(osvi.dwMajorVersion == 10) {
	    /* need to differentiate by build number */
	    if(osvi.wProductType == VER_NT_WORKSTATION) {
		if(osvi.dwBuildNumber >= 22000) desc = "11";
		else desc = "10";
	    } else {
		if(osvi.dwBuildNumber >= 20348) desc = "Server 2022";
		else if(osvi.dwBuildNumber >= 17763) desc = "Server 2019";
		else desc = "Server 2016";
	    }
	} else if(osvi.dwMajorVersion == 6) {
	    if(osvi.wProductType == VER_NT_WORKSTATION) {
		if(osvi.dwMinorVersion == 0) desc = "Vista";
		else if(osvi.dwMinorVersion == 1) desc = "7";
		else if(osvi.dwMinorVersion == 2) desc = "8";
		else if(osvi.dwMinorVersion == 3) desc = "8.1";
		else desc = "> 8.1";
	    } else {
		if(osvi.dwMinorVersion == 0) desc = "Server 2008";
		else if(osvi.dwMinorVersion == 1) desc = "Server 2008 R2";
		else if(osvi.dwMinorVersion == 2) desc = "Server 2012";
		else if(osvi.dwMinorVersion == 3) desc = "Server 2012 R2";
		else desc = "Server > 2012 R2";
	    }
	} else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
	    desc = "2000";
	else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
	    desc = "XP";
	else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
	    if(osvi.wProductType == VER_NT_WORKSTATION)
		desc = "XP Professional";
	    else
		desc = "Server 2003";
	}
	/* GetNativeSystemInfo is XP or later */
	GetNativeSystemInfo(&si);
	if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
	    type = " x64";

	if(osvi.wServicePackMajor > 0)
	    snprintf(ver, 256,
		     "Windows %s%s (build %d) Service Pack %d",
		     desc, type,
		     LOWORD(osvi.dwBuildNumber),
		     (int) osvi.wServicePackMajor);
	else
	    snprintf(ver, 256,
		     "Windows %s%s (build %d)",
		     desc, type,
		     LOWORD(osvi.dwBuildNumber));
    } else { /* should not get here */
	snprintf(ver, 256, "Windows %d.%d (build %d) %s",
		 (int) osvi.dwMajorVersion, (int) osvi.dwMinorVersion,
		 LOWORD(osvi.dwBuildNumber), osvi.szCSDVersion);
    }

    return mkString(ver);
}

SEXP dllversion(SEXP path)
{
    const wchar_t *dll;
    DWORD dwVerInfoSize;
    DWORD dwVerHnd;

    if(!isString(path) || LENGTH(path) != 1)
	error(_("invalid '%s' argument"), "path");
    dll = filenameToWchar(STRING_ELT(path, 0), FALSE);
    dwVerInfoSize = GetFileVersionInfoSizeW(dll, &dwVerHnd);
    SEXP ans = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(ans, 0, mkChar(""));
    SET_STRING_ELT(ans, 1, mkChar(""));
    if (dwVerInfoSize) {
	BOOL  fRet;
	LPSTR lpstrVffInfo;
	LPSTR lszVer = NULL;
	UINT  cchVer = 0;

	lpstrVffInfo = (LPSTR) malloc(dwVerInfoSize);
	if (GetFileVersionInfoW(dll, 0L, dwVerInfoSize, lpstrVffInfo)) {

	    fRet = VerQueryValue(lpstrVffInfo,
				 TEXT("\\StringFileInfo\\040904E4\\FileVersion"),
				 (LPVOID*)&lszVer, &cchVer);
	    if(fRet) SET_STRING_ELT(ans, 0, mkChar(lszVer));

	    fRet = VerQueryValue(lpstrVffInfo,
				 TEXT("\\StringFileInfo\\040904E4\\R Version"),
				 (LPVOID*)&lszVer, &cchVer);
	    if(fRet) SET_STRING_ELT(ans, 1, mkChar(lszVer));
	    else {
		fRet = VerQueryValue(lpstrVffInfo,
				     TEXT("\\StringFileInfo\\040904E4\\Compiled under R Version"),
				     (LPVOID*)&lszVer, &cchVer);
		if(fRet) SET_STRING_ELT(ans, 1, mkChar(lszVer));
	    }

	} else ans = R_NilValue;
	free(lpstrVffInfo);
    } else ans = R_NilValue;
    UNPROTECT(1);
    return ans;
}

SEXP getClipboardFormats(void)
{
    SEXP ans = R_NilValue;

    if(OpenClipboard(NULL)) {
	int format = 0;
	int size = CountClipboardFormats();
	PROTECT(ans = allocVector(INTSXP, size));
	for (int j = 0; j < size; j++) {
	    format = EnumClipboardFormats(format);
	    INTEGER(ans)[j] = format;
	}
	UNPROTECT(1);
	CloseClipboard();
    }
    return ans;
}

//#define STRICT_R_HEADERS
#include <R_ext/RS.h>

/* split on \r\n or just one */
static SEXP splitClipboardText(const char *s, cetype_t ienc)
{
    int cnt_r= 0, cnt_n = 0, n, nc, nl, line_len = 0;
    const char *p;
    char *line, *q, eol = '\n';
    Rboolean last = TRUE; /* does final line have EOL */
    Rboolean CRLF = FALSE;
    SEXP ans;

    for(p = s, nc = 0; *p; p++, nc++)
	switch(*p) {
	case '\n':
	    cnt_n++;
	    last = TRUE;
	    line_len = std::max(line_len, nc);
	    nc = -1;
	    break;
	case '\r':
	    cnt_r++;
	    last = TRUE;
	    break;
	default:
	    last = FALSE;
	}
    if (!last) line_len = std::max(line_len, nc);  /* the unterminated last might be the longest */
    n = std::max(cnt_n, cnt_r) + (last ? 0 : 1);
    if (cnt_n == 0 && cnt_r > 0) eol = '\r';
    if (cnt_r == cnt_n) CRLF = TRUE;
    /* over-allocate a line buffer */
    line = (char *) R_chk_calloc(1+line_len, 1);
    PROTECT(ans = allocVector(STRSXP, n));
    for(p = s, q = line, nl = 0; *p; p++) {
	if (*p == eol) {
	    *q = '\0';
	    SET_STRING_ELT(ans, nl++, mkCharCE(line, ienc));
	    q = line;
	    *q = '\0';
	} else if(CRLF && *p == '\r')
	    ;
	else *q++ = *p;
    }
    if (!last) {
	*q = '\0';
	SET_STRING_ELT(ans, nl, mkCharCE(line, ienc));
    }
    R_chk_free(line);
    UNPROTECT(1);
    return(ans);
}

SEXP readClipboard(SEXP sformat, SEXP sraw)
{
    SEXP ans = R_NilValue;
    HGLOBAL hglb;
    const char *pc;

    int format = asInteger(sformat);
    bool raw = asLogical(sraw);

    if (OpenClipboard(NULL)) {
	if (IsClipboardFormatAvailable(format) &&
	   (hglb = GetClipboardData(format)) &&
	   (pc = (const char *) GlobalLock(hglb))) {
	    if (raw) {
		Rbyte *pans;
		int size = GlobalSize(hglb);
		ans = allocVector(RAWSXP, size); /* no R allocation below */
		pans = RAW(ans);
		for (int j = 0; j < size; j++) pans[j] = *pc++;
	    } else if (format == CF_UNICODETEXT) {
		int n;
		cetype_t ienc = CE_NATIVE;
		const wchar_t *wpc = (wchar_t *) pc;
		n = wcslen(wpc);
		std::unique_ptr<char[]> tmp = std::make_unique<char[]>(4*n+1);
		char *text = tmp.get();
		R_CheckStack();
		wcstoutf8(text, wpc, sizeof(text));
		if (!strIsASCII(text)) ienc = CE_UTF8;
		ans = splitClipboardText(text, ienc);
	    } else if (format == CF_TEXT || format == CF_OEMTEXT || format == CF_DIF) {
		/* can we get the encoding out of a CF_LOCALE entry? */
		ans = splitClipboardText(pc, CE_NATIVE);
	    } else
		error("%s", _("'raw = FALSE' and format is a not a known text format"));
	    GlobalUnlock(hglb);
	}
	CloseClipboard();
    }
    return ans;
}

SEXP writeClipboard(SEXP text, SEXP sformat)
{
    HGLOBAL hglb;
    char *s;
    const char *p;
    bool success = FALSE, raw = FALSE;
    CXXR::RAllocStack::Scope rscope;

    int format = asInteger(sformat);

    if (TYPEOF(text) == RAWSXP) raw = TRUE;
    else if(!isString(text))
	error("%s", _("argument must be a character vector or a raw vector"));

    int n = length(text);
    if(n > 0) {
	int len = 0;
	if(raw) len = n;
	else if (format == CF_UNICODETEXT) {
	    len = 2; /* terminator */
	    for (int i = 0; i < n; i++)
		len += 2 * (wcslen(wtransChar(STRING_ELT(text, i))) + 2);
	} else if (format == CF_TEXT || format == CF_OEMTEXT || format == CF_DIF) {
	    len = 1; /* terminator */
	    for (int i = 0; i < n; i++)
		len += strlen(translateChar(STRING_ELT(text, i))) + 2;
	} else
	    error("%s", _("'raw = FALSE' and format is a not a known text format"));

	if ( (hglb = GlobalAlloc(GHND, len)) &&
	     (s = (char *)GlobalLock(hglb)) ) {
	    if(raw)
		for (int i = 0; i < n; i++) *s++ = RAW(text)[i];
	    else if (format == CF_UNICODETEXT) {
		const wchar_t *wp;
		wchar_t *ws = (wchar_t *) s;
		for (int i = 0; i < n; i++) {
		    wp = wtransChar(STRING_ELT(text, i));
		    while(*wp) *ws++ = *wp++;
		    *ws++ = L'\r'; *ws++ = L'\n';
		}
		*ws = L'\0';
	    } else {
		for (int i = 0; i < n; i++) {
		    p = translateChar(STRING_ELT(text, i));
		    while(*p) *s++ = *p++;
		    *s++ = '\r'; *s++ = '\n';
		}
		*s = '\0';
	    }

	    GlobalUnlock(hglb);
	    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
		warning("%s", _("unable to open the clipboard"));
		GlobalFree(hglb);
	    } else {
		success = SetClipboardData(format, hglb) != 0;
		if(!success) {
		    warning("%s", _("unable to write to the clipboard"));
		    GlobalFree(hglb);
		}
		CloseClipboard();
	    }
	}
    }

    return ScalarLogical(success);
}

#include <Startup.h>

#include <graphapp/ga.h>
#include "rui.h"

SEXP getIdentification(void)
{
    const char *res = "" /* -Wall */;

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) res = "RGui"; else res = "R Console";
	break;
    case RTerm:
	res = "Rterm";
	break;
    default:
	/* do nothing */
	break; /* -Wall */
    }
    return mkString(res);
}

SEXP getWindowTitle(void)
{
    char buf[512];
    const char *res = "";

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) res = GA_gettext(RFrame);
	else res = GA_gettext(RConsole);
	break;
    case RTerm:
	GetConsoleTitle(buf, 512);
	buf[511] = '\0';
	res = buf;
	break;
    default:
	/* do nothing */
	break;
    }
    return mkString(res);
}


static SEXP in_setTitle(const char *title)
{
    SEXP result = getWindowTitle();

    switch(CharacterMode) {
    case RGui:
	if(RguiMDI & RW_MDI) settext(RFrame, title);
	else settext(RConsole, title);
	break;
    case RTerm:
	SetConsoleTitle(title);
	break;
    default:
	/* do nothing */
	break; /* -Wall */
    }
    return result;
}

SEXP setWindowTitle(SEXP title)
{
    if(!isString(title)  || LENGTH(title) != 1 ||
       STRING_ELT(title, 0) == NA_STRING)
	error("%s", _("'title' must be a character string"));
    return in_setTitle(translateChar(STRING_ELT(title, 0)));
}


SEXP setStatusBar(SEXP text)
{
    if(!isString(text)  || LENGTH(text) != 1 ||
       STRING_ELT(text, 0) == NA_STRING)
	error("%s", _("'text' must be a character string"));
    showstatusbar();
    setstatus(translateChar(STRING_ELT(text, 0)));
    return R_NilValue;
}

static void * getConsoleHandle(const char *which)
{
    if (CharacterMode != RGui) return(NULL);
    else if (streql(which, "Console") && RConsole)
	return getHandle(RConsole);
    else if (streql(which, "Frame") && RFrame)
	return getHandle(RFrame);
    else if (streql(which, "Process"))
	return GetCurrentProcess();
    else return NULL;
}

#include <R_ext/GraphicsEngine.h>
#include "devWindows.h"
static void *getDeviceHandle(int dev)
{
    pGEDevDesc gdd;
    gadesc *xd;

    if (dev == -1) return(getHandle(RConsole));
    if (dev < 1 || dev > R_MaxDevices || dev == NA_INTEGER) return(0);
    gdd = GEgetDevice(dev - 1);
    if (!gdd) return(NULL);
    xd = (gadesc *) gdd->dev->deviceSpecific;
    if (!xd) return(NULL);
    return getHandle(xd->gawin);
}


SEXP getWindowsHandle(SEXP which)
{
    void * handle;

    if(length(which) != 1) error(_("'%s' must be length 1"), "which");
    if (isString(which)) handle = getConsoleHandle(CHAR(STRING_ELT(which,0)));
    else if (isInteger(which)) handle = getDeviceHandle(INTEGER(which)[0]);
    else handle = NULL;

    if (handle)
	return R_MakeExternalPtr(handle,R_NilValue,R_NilValue);
    else
	return R_NilValue;
}

static GCRoot<>      EnumResult;
static int           EnumCount;
static bool          EnumMinimized;
static DWORD         EnumProcessId;

static BOOL CALLBACK EnumWindowsProc(HWND handle, LPARAM param) 
{
    char title[1024];
    if (IsWindowVisible(handle)) {
    	if (EnumProcessId) { /* restrict to R windows only */
    	    DWORD processId;
    	    GetWindowThreadProcessId(handle, &processId);
    	    if (processId != EnumProcessId) return TRUE;
    	}
    	if (!EnumMinimized && IsIconic(handle)) return TRUE;
    	if (EnumCount >= length(EnumResult)) {
    	    int newlen = 2*length(EnumResult);
    	    EnumResult = lengthgets(EnumResult, newlen);
    	    setAttrib(EnumResult, R_NamesSymbol, 
    	              lengthgets(getAttrib(EnumResult, R_NamesSymbol), newlen));
    	}
    	SET_VECTOR_ELT(EnumResult, EnumCount, R_MakeExternalPtr(handle,R_NilValue,R_NilValue));
    	if (GetWindowText(handle, title, 1024)) 
    	    SET_STRING_ELT(getAttrib(EnumResult, R_NamesSymbol), EnumCount, mkChar(title));
    	EnumCount++;
    }
    return TRUE;
}

SEXP getWindowsHandles(SEXP which, SEXP minimized)
{
    EnumResult = allocVector(VECSXP, 8);
    setAttrib(EnumResult, R_NamesSymbol, allocVector(STRSXP, 8));
    EnumCount = 0;
    const char * w;

    w = CHAR(STRING_ELT(which, 0));
    EnumMinimized = asLogical(minimized);

    if (streql(w, "R")) EnumProcessId = GetCurrentProcessId();
    else EnumProcessId = 0;

    if (ismdi() && EnumProcessId) 
    	EnumChildWindows(GetParent((HWND) getHandle(RConsole)), EnumWindowsProc, 0);
    else
    	EnumWindows(EnumWindowsProc, 0);

    EnumResult = lengthgets(EnumResult, EnumCount);

    return EnumResult;
}

static void in_ArrangeWindows(int n, void** windows, int action, int preserve, int outer) 
{
    if (action == MINIMIZE || action == RESTORE) {
    	for (int j=0; j<n; j++)
    	    ShowWindow((HWND)windows[j], action == MINIMIZE ? SW_MINIMIZE : SW_RESTORE);
    } else {
    	RECT rect = {0,0,0,0};
    	RECT *prect = &rect;
    	HWND parent;
    	if (preserve) {
	    WINDOWPLACEMENT wp;
	    wp.length = sizeof(wp);
	    for (int j=0; j<n; j++) {
		if (GetWindowPlacement((HWND)windows[j], &wp)) {
		    UnionRect(prect, prect, &wp.rcNormalPosition);
		    if (wp.showCmd == SW_SHOWMINIMIZED || wp.showCmd == SW_SHOWMAXIMIZED) {
			wp.showCmd = SW_RESTORE;
			SetWindowPlacement((HWND)windows[j], &wp);
		    }
		}
	    }
	}
        if (rect.left == rect.right || rect.top == rect.bottom) prect = NULL;

        if (!outer && ismdi())
            parent = GetParent((HWND) getHandle(RConsole));
        else
            parent = NULL;
	switch (action) {
	case CASCADE: CascadeWindows(parent, 0, prect, n, (HWND FAR *)windows);
		      break;
	case TILEHORIZ: TileWindows(parent, MDITILE_HORIZONTAL, prect, n, (HWND FAR *)windows);
		      break;
	case TILEVERT: TileWindows(parent, MDITILE_VERTICAL, prect, n, (HWND FAR *)windows);
		      break;    
        }
    }
}

SEXP arrangeWindows(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP windows;
    int action, preserve, outer;

    args = CDR(args);
    windows = CAR(args);
    if (length(windows)) {
	if (TYPEOF(windows) != VECSXP) error(_("'%s' must be a list"), "windows");
	void **handles = (void **) R_alloc(length(windows), sizeof(void *));
	for (int i = 0; i < length(windows); i++) {
	    if (TYPEOF(VECTOR_ELT(windows, i)) != EXTPTRSXP)
		error(_("'%s' element %d is not a window handle"), "windows", i+1);
	    handles[i] = R_ExternalPtrAddr(VECTOR_ELT(windows, i));
	}
	action = asInteger(CADR(args));
	preserve = asInteger(CADDR(args));
	outer = asInteger(CADDDR(args));
	in_ArrangeWindows(length(windows), handles, action, preserve, outer);
    }
    return windows;
}

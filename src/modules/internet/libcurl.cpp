/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2015-2023 The R Core Team
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
# include <config.h>
#endif

#include <vector>
#include <CXXR/RAllocStack.hpp>
#include <R_ext/Minmax.h>
#define R_USE_SIGNALS 1
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <cerrno>

#ifdef HAVE_UNISTD_H
// for unlink
# include <unistd.h>
#endif

// using namespace std; // conflicts with curl.h header
using namespace R;

#ifdef HAVE_LIBCURL
# include <curl/curl.h>
/*
  This needed libcurl >= 7.28.0 (Oct 2012) for curl_multi_wait.
  Substitute code is provided for a Unix-alike only.

  There is a configure test but it is not used on Windows and system
  software can change.
*/
# ifdef Win32
#  if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
#  error libcurl 7.28.0 or later is required.
#  endif
# else
#  if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 22)
#  error libcurl 7.22.0 or later is required.
#  endif
# endif
extern void Rsleep(double timeint);
#endif

static int current_timeout = 0;

# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)

// curl/curl.h includes <sys/select.h> and headers it requires.

#define curl_multi_wait R_curl_multi_wait


static CURLMcode
R_curl_multi_wait(CURLM *multi_handle,
		  /* IGNORED */ void *unused,
		  /* IGNORED */ unsigned int extra,
		  int timeout_ms, int *ret)
{
    fd_set fdread;
    fd_set fdwrite;
    fd_set fdexcep;
    FD_ZERO(&fdread);
    FD_ZERO(&fdwrite);
    FD_ZERO(&fdexcep);

    struct timeval timeout;

    timeout.tv_sec = timeout_ms / 1000;
    timeout.tv_usec = (timeout_ms % 1000) * 1000;

    int maxfd = -1;
    CURLMcode
	mc = curl_multi_fdset(multi_handle, &fdread, &fdwrite, &fdexcep, &maxfd);
    if (maxfd == -1) {
	*ret = 0;
	Rsleep(0.1);
    } else
	/* file descriptors should be checked against FD_SETSIZE by caller */
	*ret = select(maxfd+1, &fdread, &fdwrite, &fdexcep, &timeout);

    return mc;
}
#endif

attribute_hidden SEXP in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
#ifdef HAVE_LIBCURL
    curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);
    SET_STRING_ELT(ans, 0, mkChar(d->version));
    SEXP sSSLVersion = install("ssl_version");
    setAttrib(ans, sSSLVersion,
	      mkString(d->ssl_version ? d->ssl_version : "none"));
    SEXP sLibSSHVersion = install("libssh_version");
    setAttrib(ans, sLibSSHVersion,
	      mkString(((d->age >= 3) && d->libssh_version) ? d->libssh_version : ""));
    const char * const *p;
    int n, i;
    for (p = d->protocols, n = 0; *p; p++, n++) ;
    SEXP protocols = PROTECT(allocVector(STRSXP, n));
    for (p = d->protocols, i = 0; i < n; i++, p++)
	SET_STRING_ELT(protocols, i, mkChar(*p));
    setAttrib(ans, install("protocols"), protocols);
    UNPROTECT(1);
#else
    SET_STRING_ELT(ans, 0, mkChar(""));
#endif
    UNPROTECT(1);
    return ans;
}

#ifdef HAVE_LIBCURL
static const char *http_errstr(const long status)
{
    const char *str;
    switch(status) {
    case 400: str = "Bad Request"; break;
    case 401: str = "Unauthorized"; break;
    case 402: str = "Payment Required"; break;
    case 403: str = "Forbidden"; break;
    case 404: str = "Not Found"; break;
    case 405: str = "Method Not Allowed"; break;
    case 406: str = "Not Acceptable"; break;
    case 407: str = "Proxy Authentication Required"; break;
    case 408: str = "Request Timeout"; break;
    case 409: str = "Conflict"; break;
    case 410: str = "Gone"; break;
    case 411: str = "Length Required"; break;
    case 412: str = "Precondition Failed"; break;
    case 413: str = "Request Entity Too Large"; break;
    case 414: str = "Request-URI Too Long"; break;
    case 415: str = "Unsupported Media Type"; break;
    case 416: str = "Requested Range Not Satisfiable"; break;
    case 417: str = "Expectation Failed"; break;
    case 500: str = "Internal Server Error"; break;
    case 501: str = "Not Implemented"; break;
    case 502: str = "Bad Gateway"; break;
    case 503: str = "Service Unavailable"; break;
    case 504: str = "Gateway Timeout"; break;
    default: str = "Unknown Error"; break;
    }
    return str;
}

static const char *ftp_errstr(const long status)
{
    const char *str;
    switch (status) {
    case 421: str = "Service not available, closing control connection"; break;
    case 425: str = "Cannot open data connection"; break;
    case 426: str = "Connection closed; transfer aborted"; break;
    case 430: str = "Invalid username or password"; break;
    case 434: str = "Requested host unavailable"; break;
    case 450: str = "Requested file action not taken"; break;
    case 451: str = "Requested action aborted; local error in processing"; break;
    case 452:
	str = "Requested action not taken; insufficient storage space in system";
	break;
    case 501: str = "Syntax error in parameters or arguments"; break;
    case 502: str = "Command not implemented"; break;
    case 503: str = "Bad sequence of commands"; break;
    case 504: str = "Command not implemented for that parameter"; break;
    case 530: str = "Not logged in"; break;
    case 532: str = "Need account for storing files"; break;
    case 550:
	str = "Requested action not taken; file unavailable";
	break;
    case 551: str = "Requested action aborted; page type unknown"; break;
    case 552:
	str = "Requested file action aborted; exceeded storage allocation";
	break;
    case 553: str = "Requested action not taken; file name not allowed"; break;
    default: str = "Unknown Error"; break;
    }
    return str;
}

/*
  Check curl_multi_info_read for errors, reporting as warnings

  Return: number of errors encountered
 */
static int curlMultiCheckerrs(CURLM *mhnd)
{
    int retval = 0;
    for(int n = 1; n > 0;) {
	CURLMsg *msg = curl_multi_info_read(mhnd, &n);
	if (msg && (msg->data.result != CURLE_OK)) {
	    const char *url, *strerr, *type;
	    long status = 0;
	    curl_easy_getinfo(msg->easy_handle, CURLINFO_EFFECTIVE_URL, &url);
	    curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE,
			      &status);
	    // This reports the redirected URL
	    if (status >= 400) {
		if (url && url[0] == 'h') {
		    strerr = http_errstr(status);
		    type = "HTTP";
		} else {
		    strerr = ftp_errstr(status);
		    type = "FTP";
		}
		warning(_("cannot open URL '%s': %s status was '%ld %s'"),
			url, type, status, strerr);
	    } else {
		strerr = curl_easy_strerror(msg->data.result);
		if (streql(strerr, "Timeout was reached"))
		    warning(_("URL '%s': Timeout of %d seconds was reached"),
			    url, current_timeout);
		else
		    warning(_("URL '%s': status was '%s'"), url, strerr);
	    }
	    retval++;
	}
    }
    return retval;
}

static void curlCommon(CURL *hnd, bool redirect, bool verify)
{
    const char *capath = getenv("CURL_CA_BUNDLE");
    if (verify) {
#ifdef Win32
	struct curl_tlssessioninfo *tls_backend_info = NULL;
	CURLcode ret = curl_easy_getinfo(hnd, CURLINFO_TLS_SSL_PTR,
	                                 &tls_backend_info);
	if (!ret && tls_backend_info->backend == CURLSSLBACKEND_SCHANNEL) {
	    capath = NULL;
# if LIBCURL_VERSION_NUM >= 0x074600
	    const char *rbe = getenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT");
	    if (rbe && StringTrue(rbe)) 
		curl_easy_setopt(hnd, CURLOPT_SSL_OPTIONS,
				 CURLSSLOPT_REVOKE_BEST_EFFORT);
# endif
	}
#endif
	if (capath && capath[0])
	    curl_easy_setopt(hnd, CURLOPT_CAINFO, capath);
    } else {
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYHOST, 0L);
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYPEER, 0L);
    }
#if 0
    // for consistency, but all utils:::makeUserAgent does is look up an option.
    SEXP sMakeUserAgent = install("makeUserAgent");
    SEXP agentFun = PROTECT(lang2(sMakeUserAgent, ScalarLogical(0)));
    SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
    SEXP sua = eval(agentFun, utilsNS);
    UNPROTECT(1); /* utilsNS */
    PROTECT(sua);
    if(TYPEOF(sua) != NILSXP)
	curl_easy_setopt(hnd, CURLOPT_USERAGENT, CHAR(STRING_ELT(sua, 0)));
    UNPROTECT(2);
#else
    int Default = 1;
    SEXP sua = GetOption1(install("HTTPUserAgent")); // set in utils startup
    if (TYPEOF(sua) == STRSXP && LENGTH(sua) == 1 ) {
	CXXR::RAllocStack::Scope rscope;
	const char *p = translateChar(STRING_ELT(sua, 0));
	if (p[0] && p[1] && p[2] && p[0] == 'R' && p[1] == ' ' && p[2] == '(') {
	} else {
	    Default = 0;
	    curl_easy_setopt(hnd, CURLOPT_USERAGENT, p);
	}
    }
    if (Default) {
	char buf[20];
	curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);
	snprintf(buf, 20, "libcurl/%s", d->version);
	curl_easy_setopt(hnd, CURLOPT_USERAGENT, buf);
    }
#endif
    int timeout0 = asInteger(GetOption1(install("timeout")));
    long timeout = (timeout0 == NA_INTEGER) ? 0 : (1000L * timeout0);
    current_timeout = (timeout0 == NA_INTEGER) ? 0 : timeout0;
    curl_easy_setopt(hnd, CURLOPT_CONNECTTIMEOUT_MS, timeout);
    curl_easy_setopt(hnd, CURLOPT_TIMEOUT_MS, timeout);
    if (redirect) {
	curl_easy_setopt(hnd, CURLOPT_FOLLOWLOCATION, 1L);
	curl_easy_setopt(hnd, CURLOPT_MAXREDIRS, 20L);
    }
    int verbosity = asInteger(GetOption1(install("internet.info")));
    if (verbosity < 2) curl_easy_setopt(hnd, CURLOPT_VERBOSE, 1L);

    // enable the cookie engine, keep cookies in memory
    curl_easy_setopt(hnd, CURLOPT_COOKIEFILE, "");
}

static char headers[500][2049]; // allow for terminator
static int used;

static size_t rcvHeaders(void *buffer, size_t size, size_t nmemb, void *userp)
{
    char *d = (char*)buffer;
    size_t result = size * nmemb, res = result > 2048 ? 2048 : result;
    if (used >= 500) return result;
    strncpy(headers[used], d, res);
    // 'Do not assume that the header line is zero terminated!'
    headers[used][res] = '\0';
    used++;
    return result;
}

static size_t rcvBody(void *buffer, size_t size, size_t nmemb, void *userp)
{
    // needed to discard spurious ftp 'body' otherwise written to stdout
    return size * nmemb;
}
#endif

static void handle_cleanup(void *data)
{
    CURL *hnd = (CURL *) data;
    if (hnd)
	curl_easy_cleanup(hnd);
}

attribute_hidden
SEXP in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_LIBCURL
    error("%s", _("curlGetHeaders is not supported on this platform"));
    return R_NilValue;
#else
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
       error("invalid %s argument", "url");
    const char *url = translateChar(STRING_ELT(CAR(args), 0));
    used = 0;
    bool redirect = asLogicalNoNA(CADR(args), "redirect");
    bool verify = asLogicalNoNA(CADDR(args), "verify");
    int timeout = asInteger(CADDDR(args));
    if (timeout == NA_INTEGER)
	error(_("invalid %s argument"), "timeout");
    SEXP sTLS = CAD4R(args);
    const char *TLS = "";
    if (isString(sTLS) && LENGTH(sTLS) == 1 && STRING_ELT(sTLS, 0) != NA_STRING)
	TLS = translateChar(STRING_ELT(sTLS, 0));
    else error(_("invalid %s argument"), "TLS");

    CURL *hnd = curl_easy_init();
    if (!hnd)
	error("%s", _("could not create curl handle"));
    long http_code = 0;
    /* Set up a context which will free the handle on error (also from
       curlCommon) */
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &handle_cleanup;
    cntxt.cenddata = hnd;
    curl_easy_setopt(hnd, CURLOPT_URL, url);
    curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(hnd, CURLOPT_NOBODY, 1L);
    curl_easy_setopt(hnd, CURLOPT_HEADERFUNCTION, &rcvHeaders);
    curl_easy_setopt(hnd, CURLOPT_WRITEHEADER, &headers);
    /* libcurl (at least 7.40.0) does not respect CURLOPT_NOBODY
       for some ftp header info (Content-Length and Accept-ranges). */
    curl_easy_setopt(hnd, CURLOPT_WRITEFUNCTION, &rcvBody);
    curlCommon(hnd, redirect, verify);
    if (timeout > 0) {
	curl_easy_setopt(hnd, CURLOPT_TIMEOUT, timeout);
	current_timeout = timeout;
    }
    if (!streql(TLS, "")) {
	// 7.34.0 was released 2013-12-17
#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 34)
	long TLS_ver = CURL_SSLVERSION_TLSv1_0;
	if (streql(TLS, "1.0")) TLS_ver = CURL_SSLVERSION_TLSv1_0; 
	else if (streql(TLS, "1.1")) TLS_ver = CURL_SSLVERSION_TLSv1_1; 
	else if (streql(TLS, "1.2")) TLS_ver = CURL_SSLVERSION_TLSv1_2;
# if LIBCURL_VERSION_MAJOR > 7 ||  (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 52)
	else if (streql(TLS, "1.3")) TLS_ver = CURL_SSLVERSION_TLSv1_3;
# endif
	else
	    error(_("invalid %s argument"), "TLS");
	curl_easy_setopt(hnd, CURLOPT_SSLVERSION, TLS_ver);
# else
	error("TLS argument is unsupported in this libcurl version %d.%d",
	      LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR);
#endif
    }

    char errbuf[CURL_ERROR_SIZE];
    curl_easy_setopt(hnd, CURLOPT_ERRORBUFFER, errbuf);
    // libcurl does not initialize this
    errbuf[0] = '\0';
    CURLcode ret = curl_easy_perform(hnd);
    if (ret != CURLE_OK) {
	if (errbuf[0])
	    error(_("libcurl error code %d:\n\t%s\n"), ret, errbuf);
	else if(ret == 77)
	    error(_("libcurl error code %d:\n\t%s\n"), ret,
		  "unable to access SSL/TLS CA certificates");
	else // rare case, error but no message
	    error("libcurl error code %d\n", ret);
    }
    // long http_code = 0;
    curl_easy_getinfo (hnd, CURLINFO_RESPONSE_CODE, &http_code);
    endcontext(&cntxt);
    curl_easy_cleanup(hnd);

    SEXP ans = PROTECT(allocVector(STRSXP, used));
    for (int i = 0; i < used; i++)
	SET_STRING_ELT(ans, i, mkChar(headers[i]));
    SEXP sStatus = install("status");
    setAttrib(ans, sStatus, ScalarInteger((int) http_code));
    UNPROTECT(1);
    return ans;
#endif
}

#ifdef HAVE_LIBCURL
static double total;

static int ndashes;
static void putdashes(int *pold, int new_)
{
    for (int i = *pold; i < new_; i++)  REprintf("=");
    if (R_Consolefile) fflush(R_Consolefile);
    *pold = new_;
}

# ifdef Win32
#include <ga.h>
#undef resize // graphapp defines resize as GA_resize
/* We could share this window with internet.c, then re-positioning
   would apply to both */
typedef struct {
    window wprog;
    progressbar pb;
    label l_url;
    int pc;
} winprogressbar;

static winprogressbar pbar = {NULL, NULL, NULL};
# endif // Win32

#if LIBCURL_VERSION_NUM >= 0x072000
# define CURL_LEN curl_off_t
#else
# define CURL_LEN double
#endif

static
int progress(void *clientp, CURL_LEN dltotal, CURL_LEN dlnow,
	     CURL_LEN ultotal, CURL_LEN ulnow)
{
    CURL *hnd = (CURL *) clientp;
    long status;
    curl_easy_getinfo(hnd, CURLINFO_RESPONSE_CODE, &status);

# ifdef Win32
    static int factor = 1;
# endif

    // we only use downloads.  dltotal may be zero.
    if ((status < 300) && (dltotal > 0.)) {
	if (total == 0.) {
	    total = dltotal;
	    char *type = NULL;
	    curl_easy_getinfo(hnd, CURLINFO_CONTENT_TYPE, &type);
	    REprintf("Content type '%s'", type ? type : "unknown");
	    if (total > 1024.0*1024.0)
		// might be longer than long, and is on 64-bit windows
		REprintf(" length %0.0f bytes (%0.1f MB)\n",
			 total, total/1024.0/1024.0);
	    else if (total > 10240)
		REprintf(" length %d bytes (%d KB)\n",
			 (int)total, (int)(total/1024));
	    else
		REprintf(" length %d bytes\n", (int)total);
# ifdef Win32
	    R_FlushConsole();
	    if(R_Interactive) {
		if (total > 1e9) factor = total/1e6; else factor = 1;
		setprogressbarrange(pbar.pb, 0, total/factor);
		show(pbar.wprog);
	    }
	}
	if (R_Interactive) {
	    setprogressbar(pbar.pb, 1.0*dlnow/factor);
	    if (total > 0) {
		static char pbuf[30];
		int pc = 0.499 + 100.0*dlnow/total;
		if (pc > pbar.pc) {
		    snprintf(pbuf, 30, "%d%% downloaded", pc);
		    settext(pbar.wprog, pbuf);
		    pbar.pc = pc;
		}
	    }
	} else putdashes(&ndashes, (int)(50.0*dlnow/total));
    }
    R_ProcessEvents();
    return 0;
# else

	    if (R_Consolefile) fflush(R_Consolefile);
	}
	putdashes(&ndashes, (int)(50.0*dlnow/total));
    }
    return 0;
# endif
}
#endif // HAVE_LIBCURL

typedef struct {
    struct curl_slist *headers;
    CURLM *mhnd;
    int nurls;
    CURL **hnd;
    FILE **out;
    SEXP sfile;
#ifdef Win32
    winprogressbar *pbar;
#endif
} download_cleanup_info;

static void download_cleanup(void *data)
{
    download_cleanup_info *c = (download_cleanup_info *)data;

    for (int i = 0; i < c->nurls; i++) {
	if (c->out && c->out[i]) {
	    fclose(c->out[i]);
#if LIBCURL_VERSION_NUM >= 0x073700
	    curl_off_t dl;
	    curl_easy_getinfo(c->hnd[i], CURLINFO_SIZE_DOWNLOAD_T, &dl);
#else
	    double dl;
	    curl_easy_getinfo(c->hnd[i], CURLINFO_SIZE_DOWNLOAD, &dl);
#endif
	    if (c->sfile) {
		long status = 0L;
		curl_easy_getinfo(c->hnd[i], CURLINFO_RESPONSE_CODE, &status);
		// should we do something about incomplete transfers?
		if (status != 200 && dl == 0.) {
		    CXXR::RAllocStack::Scope rscope;
		    unlink(R_ExpandFileName(translateChar(STRING_ELT(c->sfile, i))));
		}
	    }
	    curl_multi_remove_handle(c->mhnd, c->hnd[i]);
	}
	if (c->hnd && c->hnd[i])
	    curl_easy_cleanup(c->hnd[i]);
    }
    if (c->mhnd)
	curl_multi_cleanup(c->mhnd);
    if (c->headers)
	curl_slist_free_all(c->headers);

#ifdef Win32
    if (c->pbar)
	hide(c->pbar->wprog);
#endif
}

/* download(url, destfile, quiet, mode, headers, cacheOK) */

attribute_hidden
SEXP in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_LIBCURL
    error("%s", _("download.file(method = \"libcurl\") is not supported on this platform"));
    return R_NilValue;
#else
    SEXP scmd, sfile, smode, sheaders;
    const char *url, *file, *mode;
    struct curl_slist *headers = NULL;
    CXXR::RAllocStack::Scope rscope;
    download_cleanup_info c;

    scmd = CAR(args); args = CDR(args);
    if (!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    int nurls = length(scmd);

#ifdef Win32
    /* not used as 7.28 is required */
# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
    if (nurls > FD_SETSIZE)
	error("%s", _("too many file descriptors for select()"));
# endif
#endif

    sfile = CAR(args); args = CDR(args);
    if (!isString(sfile) || length(sfile) < 1)
	error(_("invalid '%s' argument"), "destfile");
    if (length(sfile) != length(scmd))
	error("%s", _("lengths of 'url' and 'destfile' must match"));
    bool quiet = asLogicalNoNA(CAR(args), "quiet"); args = CDR(args);
    smode =  CAR(args); args = CDR(args);
    if (!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = translateChar(STRING_ELT(smode, 0));
    bool cacheOK = asLogicalNoNA(CAR(args), "cacheOK"); args = CDR(args);
    sheaders = CAR(args);
    if(TYPEOF(sheaders) != NILSXP && !isString(sheaders))
	error(_("invalid '%s' argument"), "headers");

    c.mhnd = NULL;
    c.nurls = nurls;
    c.hnd = NULL;
    c.out = NULL;
    if (strchr(mode, 'w'))
	c.sfile = sfile;
    else
	c.sfile = NULL;
#ifdef Win32
    c.pbar = NULL;
#endif
    c.headers = NULL;
    int n_err = 0;
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &download_cleanup;
    cntxt.cenddata = &c;
    if(TYPEOF(sheaders) != NILSXP) {
	for (int i = 0; i < LENGTH(sheaders); i++) {
	    struct curl_slist *tmp =
		curl_slist_append(headers,
		                  translateChar(STRING_ELT(sheaders, i)));
	    if (!tmp)
		error("%s", _("out of memory"));
	    c.headers = headers = tmp;
	}
    }

    /* This comes mainly from curl --libcurl on the call used by
       download.file(method = "curl").
       Also https://curl.haxx.se/libcurl/c/multi-single.html.
    */

    if (!cacheOK) {
	/* This _is_ the right way to do this: see §14.9 of
	   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html */
	struct curl_slist *tmp =
	    curl_slist_append(headers, "Pragma: no-cache");
	if(!tmp)
	    error("%s", _("out of memory"));
	c.headers = headers = tmp;
    }

    CURLM *mhnd = curl_multi_init();
    if (!mhnd)
	error("%s", _("could not create curl handle"));
    c.mhnd = mhnd;

    int still_running, repeats = 0;
    CURL *hnd[nurls];
    FILE *out[nurls];

    for(int i = 0; i < nurls; i++) {
	hnd[i] = NULL;
	out[i] = NULL;
    }
    c.hnd = hnd;
    c.out = out;

    for(int i = 0; i < nurls; i++) {
	url = translateChar(STRING_ELT(scmd, i));
	hnd[i] = curl_easy_init();
	if (!hnd[i]) {
	    n_err += 1;
	    warning("%s", _("could not create curl handle"));
	    continue;
	}
	curl_easy_setopt(hnd[i], CURLOPT_URL, url);
	curl_easy_setopt(hnd[i], CURLOPT_FAILONERROR, 1L);
	/* Users will normally expect to follow redirections, although
	   that is not the default in either curl or libcurl. */
	curlCommon(hnd[i], 1, 1);
	// all but Unix-alikes with ancient libcurl (before 2012-03-22)
#if (LIBCURL_VERSION_MAJOR > 7) || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 25)
	curl_easy_setopt(hnd[i], CURLOPT_TCP_KEEPALIVE, 1L);
#endif
	curl_easy_setopt(hnd[i], CURLOPT_HTTPHEADER, headers);

	/* check that destfile can be written */
	file = translateChar(STRING_ELT(sfile, i));
	out[i] = R_fopen(R_ExpandFileName(file), mode);
	if (!out[i]) {
	    n_err += 1;
	    warning(_("URL %s: cannot open destfile '%s', reason '%s'"),
		    url, file, strerror(errno));
	    continue;
	}
#ifdef Unix
# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
	if (fileno(out[i]) >= FD_SETSIZE) {
	    n_err += 1;
	    fclose(out[i]);
	    out[i] = NULL;
	    warning("%s", _("file descriptor is too large for select()"));
	    continue;
	}
# endif
#endif
	// This uses the internal CURLOPT_WRITEFUNCTION
	curl_easy_setopt(hnd[i], CURLOPT_WRITEDATA, out[i]);
	curl_multi_add_handle(mhnd, hnd[i]);

	total = 0.;
	if (!quiet && nurls <= 1) {
	    // It would in principle be possible to have
	    // multiple progress bars on Windows.
	    curl_easy_setopt(hnd[i], CURLOPT_NOPROGRESS, 0L);
	    ndashes = 0;
#ifdef Win32
	    if (R_Interactive) {
		if (!pbar.wprog) {
		    pbar.wprog = newwindow(_("Download progress"),
					   rect(0, 0, 540, 100),
					   Titlebar | Centered);
		    setbackground(pbar.wprog, dialog_bg());
		    pbar.l_url = newlabel(" ", rect(10, 15, 520, 25),
					  AlignCenter);
		    pbar.pb = newprogressbar(rect(20, 50, 500, 20),
					     0, 1024, 1024, 1);
		    pbar.pc = 0;
		}

		settext(pbar.l_url, url);
		setprogressbar(pbar.pb, 0);
		settext(pbar.wprog, "Download progress");
		show(pbar.wprog);
		c.pbar = &pbar;
	    }
#endif
	    // For libcurl >= 7.32.0 use CURLOPT_XFERINFOFUNCTION
#if LIBCURL_VERSION_NUM >= 0x072000
	    curl_easy_setopt(hnd[i], CURLOPT_XFERINFOFUNCTION, progress);
	    curl_easy_setopt(hnd[i], CURLOPT_XFERINFODATA, hnd[i]);
#else
	    curl_easy_setopt(hnd[i], CURLOPT_PROGRESSFUNCTION, progress);
	    curl_easy_setopt(hnd[i], CURLOPT_PROGRESSDATA, hnd[i]);
#endif
	} else curl_easy_setopt(hnd[i], CURLOPT_NOPROGRESS, 1L);

	/* This would allow the negotiation of compressed HTTP transfers,
	   but it is not clear it is always a good idea.
	   curl_easy_setopt(hnd[i], CURLOPT_ACCEPT_ENCODING, "gzip, deflate");
	*/

	if (!quiet) REprintf(_("trying URL '%s'\n"), url);
    }

    if (n_err == nurls) {
	// no dest files could be opened, so bail out
	endcontext(&cntxt);
	download_cleanup(&c);
	return ScalarInteger(1);
    }

    R_Busy(1);
    //  curl_multi_wait needs curl >= 7.28.0 .
    curl_multi_perform(mhnd, &still_running);
    do {
	int numfds;
	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds);
	if (mc != CURLM_OK)  { // internal, do not translate
	    warning("curl_multi_wait() failed, code %d", mc);
	    break;
	}
	if (!numfds) {
	    /* 'numfds' being zero means either a timeout or no file
	       descriptors to wait for. Try timeout on first
	       occurrence, then assume no file descriptors to wait for
	       means 'sleep for 100 milliseconds'.
	    */
	    if (repeats++ > 0) Rsleep(0.1); // do not block R process
	} else repeats = 0;
	R_ProcessEvents();
	curl_multi_perform(mhnd, &still_running);
    } while(still_running);
    R_Busy(0);
#ifdef Win32
    if (R_Interactive && !quiet && nurls<=1) {
	c.pbar = NULL;
	hide(pbar.wprog);
    } else if (total > 0.) {
	REprintf("\n");
	R_FlushConsole();
    }
#else
    if (total > 0.) REprintf("\n");
    if (R_Consolefile) fflush(R_Consolefile);
#endif
    if (nurls == 1) {
	long status;
	curl_easy_getinfo(hnd[0], CURLINFO_RESPONSE_CODE, &status);
	// new interface in libcurl >= 7.55.0
#if LIBCURL_VERSION_NUM >= 0x073700
	curl_off_t cl, dl;
	curl_easy_getinfo(hnd[0], CURLINFO_SIZE_DOWNLOAD_T, &dl);
#else
	double cl ,dl;
	curl_easy_getinfo(hnd[0], CURLINFO_SIZE_DOWNLOAD, &dl);
#endif
	if (!quiet && status == 200) {
	    if (dl > 1024*1024)
		REprintf("downloaded %0.1f MB\n\n", (double)dl/1024/1024);
	    else if (dl > 10240)
		REprintf("downloaded %d KB\n\n", (int) (dl/1024.0));
	    else
		REprintf("downloaded %d bytes\n\n", (int) dl);
	}
#if LIBCURL_VERSION_NUM >= 0x073700
	curl_easy_getinfo(hnd[0], CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &cl);
#else
	curl_easy_getinfo(hnd[0], CURLINFO_CONTENT_LENGTH_DOWNLOAD, &cl);
#endif
	if (cl >= 0 && dl != cl)
	    warning(_("downloaded length %0.f != reported length %0.f"),
	            (double) dl, (double) cl);
    }

    n_err += curlMultiCheckerrs(mhnd);

    if(nurls > 1) {
	if (n_err == nurls) error("%s", _("cannot download any files"));
	else if (n_err) warning("%s", _("some files were not downloaded"));
    } else if(n_err) {
	long status = 0L;
	curl_easy_getinfo(hnd[0], CURLINFO_RESPONSE_CODE, &status);	
	if (status != 200)
	    error(_("cannot open URL '%s'"),
	          translateChar(STRING_ELT(scmd, 0)));
	else
	    error(_("download from '%s' failed"),
	          translateChar(STRING_ELT(scmd, 0)));
    }
    endcontext(&cntxt);
    download_cleanup(&c);
    return ScalarInteger(0);
#endif
}

/* -------------------------- connections part ------------------------*/

/* Unfortunately the libcurl interface is not well adapted to reading
   data in user-requested chunks.

   But it does read in up to CURL_MAX_WRITE_SIZE chunks, which is 16K.
   So we implement a buffer which holds two chunks, and when what we
   have is not enough we move down what it left and fetch another
   chunk above it.  For safety, the buffer is expandable but this
   should not be exercised.

   It seems that expanding was being done by a couple of packages and
   gave a use-after-free error with libcurl 7.64.0.  So initial size
   increased to 16x.

   An alternative design would be for consumeData to return what is
   available and reset current.  Then rcvData would only be called on
   a completely empty buffer.
 */

#include <Rconnections.h>

#ifdef HAVE_LIBCURL
typedef struct Curlconn {
    char *buf, *current; // base of buffer, last read address
    size_t bufsize, filled;  // buffer size, amount which has been filled
    Rboolean available; // to be read out
    int sr; // 'still running' count
    CURLM *mh; CURL *hnd;
    struct curl_slist *headers;
} *RCurlconn;

static size_t rcvData(void *ptr, size_t size, size_t nitems, void *ctx)
{
    RCurlconn ctxt = (RCurlconn) ctx;

    /* move down any unused data: can overlap */
    if (ctxt->filled) memmove(ctxt->buf, ctxt->current, ctxt->filled);

    size_t add = size * nitems;
    if (add) {
	/* Allocate more space if required: unlikely.
	   Do so as an integer multiple of the current size.
	 */
	if (ctxt->filled + add > ctxt->bufsize) {
	    int mult = (int) ceil((double)(ctxt->filled + add)/ctxt->bufsize);
	    size_t newbufsize = mult * ctxt->bufsize;
	    void *newbuf = realloc(ctxt->buf, newbufsize);
	    if (!newbuf) error("Failure in re-allocation in rcvData");
	    ctxt->buf = (char *) newbuf; ctxt->bufsize = newbufsize;
	}

	memcpy(ctxt->buf + ctxt->filled, ptr, add);
	ctxt->filled += add;
	ctxt->available = TRUE;
    }
    ctxt->current = ctxt->buf;
    return add;
}

static size_t consumeData(void *ptr, size_t max, RCurlconn ctxt)
{
    size_t size = std::min(ctxt->filled, max);  // guaranteed > 0
    memcpy(ptr, ctxt->current, size);
    ctxt->current += size; ctxt->filled -= size;
    return size;
}

/*
  return: number of errors encountered
 */
static int fetchData(RCurlconn ctxt)
{
    int repeats = 0;
    CURLM *mhnd = ctxt->mh;

    do {
	int numfds;
	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds);
	if (mc != CURLM_OK) {
	    warning("curl_multi_wait() failed, code %d", mc);
	    break;
	}
	if (!numfds) {
	    if (repeats++ > 0) Rsleep(0.1);
	} else repeats = 0;
	curl_multi_perform(mhnd, &ctxt->sr);
	if (ctxt->available) break;
	R_ProcessEvents();
    } while(ctxt->sr);

    return curlMultiCheckerrs(mhnd);
}

static void Curl_close(Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->connprivate);

    curl_slist_free_all(ctxt->headers);
    curl_multi_remove_handle(ctxt->mh, ctxt->hnd);
    curl_easy_cleanup(ctxt->hnd);
    curl_multi_cleanup(ctxt->mh);
    con->isopen = FALSE;
}

static void Curl_destroy(Rconnection con)
{
    RCurlconn ctxt;

    if (NULL == con)
	return;
    ctxt = (RCurlconn)(con->connprivate);

    if (NULL == ctxt)
	return;

    free(ctxt->buf);
    free(ctxt);
}

static size_t Curl_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->connprivate);
    size_t nbytes = size*nitems;
    char *p = (char *) ptr;
    size_t total = consumeData(ptr, nbytes, ctxt);
    int n_err = 0;
    while((total < nbytes) && ctxt->sr) {
	/* FIXME: A an error from fetchData() or a warning turned into error
	          would not close the connection. But the GC would do it later.
	*/
	n_err += fetchData(ctxt);
	total += consumeData(p + total, (nbytes - total), ctxt);
    }
    if (n_err != 0) {
	Curl_close(con);
	error("%s", _("cannot read from connection"));
    }
    return total/size;
}

static Rboolean Curl_open(Rconnection con)
{
    char *url = con->description;
    RCurlconn ctxt = (RCurlconn)(con->connprivate);
    int mlen;

    if (con->mode[0] != 'r') {
	REprintf("can only open URLs for reading");
	return FALSE;
    }

    ctxt->hnd = curl_easy_init();
    if (!ctxt->hnd)
	error("%s", _("could not create curl handle"));
    int n_err = 0;
    /* Set up a context which will free the handle on error (also from
       curlCommon) */
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &handle_cleanup;
    cntxt.cenddata = ctxt->hnd;
    curl_easy_setopt(ctxt->hnd, CURLOPT_URL, url);
    curl_easy_setopt(ctxt->hnd, CURLOPT_FAILONERROR, 1L);
    curlCommon(ctxt->hnd, 1, 1);
    curl_easy_setopt(ctxt->hnd, CURLOPT_NOPROGRESS, 1L);
#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 25)
    curl_easy_setopt(ctxt->hnd, CURLOPT_TCP_KEEPALIVE, 1L);
#endif

    if (ctxt->headers)
	curl_easy_setopt(ctxt->hnd, CURLOPT_HTTPHEADER, ctxt->headers);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEFUNCTION, rcvData);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEDATA, ctxt);
    ctxt->mh = curl_multi_init();
    if (!ctxt->mh) 
	error("%s", _("could not create curl handle"));
    /* FIXME: suitability of file handle for select should be checked with
              libcurl older than 7.28 */
    curl_multi_add_handle(ctxt->mh, ctxt->hnd);

    ctxt->current = ctxt->buf; ctxt->filled = 0; ctxt->available = FALSE;

    // Establish the connection: not clear if we should do this now.
    ctxt->sr = 1;
    n_err = 0;
    endcontext(&cntxt); /* from now leave ctxt->hnd cleanup to GC */
    con->isopen = TRUE; /* enable GC cleanup of opened connections */
    while(ctxt->sr && !ctxt->available)
	/* FIXME: A an error from fetchData() or a warning turned into error
	          would not close the connection. But the GC would do it later.
	*/
	n_err += fetchData(ctxt);
    if (n_err != 0) {
	Curl_close(con);
	error(_("cannot open the connection to '%s'"), url);
    }

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if (mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static int Curl_fgetc_internal(Rconnection con)
{
    unsigned char c;
    size_t n = Curl_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}
#endif


// 'type' is unused.
Rconnection in_newCurlUrl(const char *description, const char * const mode,
	      SEXP headers, int type)
{
#ifdef HAVE_LIBCURL
    Rconnection new_ = (Rconnection) malloc(sizeof(struct Rconn));
    if (!new_) error("%s", _("allocation of url connection failed"));
    new_->connclass = (char *) malloc(strlen("url-libcurl") + 1);
    if (!new_->connclass) {
	free(new_);
	error("%s", _("allocation of url connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    strcpy(new_->connclass, "url-libcurl");
    new_->description = (char *) malloc(strlen(description) + 1);
    if (!new_->description) {
	free(new_->connclass); free(new_);
	error("%s", _("allocation of url connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    init_con(new_, description, CE_NATIVE, mode);
    new_->canwrite = FALSE;
    new_->open = &Curl_open;
    new_->close = &Curl_close;
    new_->destroy = &Curl_destroy;
    new_->fgetc_internal = &Curl_fgetc_internal;
    new_->fgetc = &dummy_fgetc;
    new_->read = &Curl_read;
    new_->connprivate = (void *) malloc(sizeof(struct Curlconn));
    if (!new_->connprivate) {
	free(new_->description); free(new_->connclass); free(new_);
	error("%s", _("allocation of url connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    RCurlconn ctxt = (RCurlconn) new_->connprivate;
    ctxt->bufsize = 16 * CURL_MAX_WRITE_SIZE;
    ctxt->buf = (char *) malloc(ctxt->bufsize);
    if (!ctxt->buf) {
	free(new_->description); free(new_->connclass); free(new_->connprivate);
	free(new_);
	error("%s", _("allocation of url connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    ctxt->headers = NULL;
    CXXR::RAllocStack::Scope rscope;
    for (int i = 0; i < LENGTH(headers); i++) {
	struct curl_slist *tmp =
	    curl_slist_append(ctxt->headers,
	                      translateChar(STRING_ELT(headers, i)));
	if (!tmp) {
	    free(new_->description); free(new_->connclass); free(new_->connprivate);
	    free(new_); curl_slist_free_all(ctxt->headers);
	    error("%s", _("allocation of url connection failed"));
	    /* for Solaris 12.5 */ new_ = NULL;
	}
	ctxt->headers = tmp;
    }
    return new_;
#else
    error("%s", _("url(method = \"libcurl\") is not supported on this platform"));
    return (Rconnection)0; /* -Wall */
#endif
}
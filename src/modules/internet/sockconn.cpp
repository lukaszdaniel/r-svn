/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001-2024   The R Core Team.
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

/* <UTF8> chars are only handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/* ------------------- socket connections  --------------------- */
#include <cerrno>
#include "sock.h"
#include <CXXR/RContext.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Rconnections.h>

using namespace R;
using namespace CXXR;

static Rboolean sock_open(Rconnection con)
{
    Rsockconn this_ = (Rsockconn)con->connprivate;
    int sock, sock1, mlen;
    int timeout = this_->timeout;
    char buf[256];

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    this_->pend = this_->pstart = this_->inbuf;

    if(this_->server) {
	if (this_->serverfd == -1) {
	    sock1 = R_SockOpen(this_->port); /* socket(), bind(), listen() */
	    if(sock1 < 0) {
		warning(_("port %d cannot be opened"), this_->port);
		return FALSE;
	    }
#ifdef Unix
	    if (sock1 >= FD_SETSIZE) {
		/* R_SockListen below would fail */
		R_SockClose(sock1);
		warning("%s", _("file descriptor is too large for select()"));
		return FALSE;
	    }
#endif
	    /* set up a context which will close socket on jump. */
	    try {
		sock = R_SockListen(sock1, buf, 256, timeout); /* accept() */
	    } catch (...)
	    {
            R_SockClose(sock1);
            throw;
	    }
	    R_SockClose(sock1);
	    if(sock < 0) {
		/* NOTE: potentially confusing as the error was in accept() */
		warning("%s", _("problem in listening on this socket"));
		return FALSE;
	    }
	} else {
	    /* accept() */
	    sock = R_SockListen(this_->serverfd, buf, 256, timeout);
	    if(sock < 0) {
		/* "accepting" as this is used with socketAccept() */
		warning("%s", _("problem in accepting connections on this socket"));
		return FALSE;
	    }
	}
#ifdef Unix
	if (sock >= FD_SETSIZE && (con->canwrite || con->blocking)) {
	    /* Reading/writing via such socket would fail */
	    R_SockClose(sock);
	    warning("%s", _("file descriptor is too large for select()"));
	    return FALSE;
	}
#endif
	free(con->description);
	size_t sz = strlen(buf) + 10;
	con->description = (char *) malloc(sz); // FIXME check allocation 
	snprintf(con->description, sz, "<-%s:%d", buf, this_->port);
    } else {
	sock = R_SockConnect(this_->port, con->description, timeout);
	if(sock < 0) {
	    warning(_("%s:%d cannot be opened"), con->description, this_->port);
	    return FALSE;
	}
	snprintf(buf, 256, "->%s:%d", con->description, this_->port);
	strcpy(con->description, buf);
    }
    this_->fd = sock;

    if (this_->options & RSC_SET_TCP_NODELAY)
	R_set_nodelay(sock);

    mlen = (int) strlen(con->mode);
    con->isopen = TRUE;
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con); /* OK for output, at least */
    con->save = -1000;
    return TRUE;
}

static void sock_close(Rconnection con)
{
    Rsockconn this_ = (Rsockconn)con->connprivate;
    R_SockClose(this_->fd);
    con->isopen = FALSE;
}

static void servsock_close(Rconnection con)
{
    Rservsockconn this_ = (Rservsockconn)con->connprivate;
    R_SockClose(this_->fd);
    con->isopen = FALSE;
}

static ssize_t sock_read_helper(Rconnection con, void *ptr, size_t size)
{
    Rsockconn this_ = (Rsockconn)con->connprivate;
    ssize_t res;
    size_t nread = 0, n;

    con->incomplete = FALSE;
    do {
	/* read data into the buffer if it's empty and size > 0 */
	if (size > 0 && this_->pstart == this_->pend) {
	    this_->pstart = this_->pend = this_->inbuf;
	    do
		res = R_SockRead(this_->fd, this_->inbuf, 4096, 
				 con->blocking, this_->timeout);
#ifdef Win32
	    while (-res == WSAEINTR);
	    if (! con->blocking && -res == WSAEWOULDBLOCK) {
#else
	    while (-res == EINTR);
	    if (! con->blocking && (-res == EAGAIN || -res == EWOULDBLOCK)) {
#endif
		con->incomplete = TRUE;
		return nread;
	    }
	    else if (res == 0) /* should mean EOF */
		return nread;
	    else if (res < 0) return res;
	    else this_->pend = this_->inbuf + res;
	}

	/* copy data from buffer to ptr */
	if (this_->pstart + size <= this_->pend)
	    n = size;
	else
	    n = this_->pend - this_->pstart;
	if (n)
	    memcpy(ptr, this_->pstart, n);
	ptr = ((char *) ptr) + n;
	this_->pstart += n;
	size -= n;
	nread += n;
    } while (size > 0);

    return nread;
}


static int sock_fgetc_internal(Rconnection con)
{
    unsigned char c;
    ssize_t n;

    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t sock_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    ssize_t n = sock_read_helper(con, ptr, size * nitems)/((ssize_t)size);
    return n > 0 ? n : 0;
}

static size_t sock_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rsockconn this_ = (Rsockconn)con->connprivate;
    ssize_t n = R_SockWrite(this_->fd, ptr, (size_t)(size * nitems),
			    this_->timeout)/((ssize_t)size);
    return n > 0 ? n : 0;
}

Rconnection in_R_newsock(const char *host, int port, int server, int serverfd,
			 const char * const mode, int timeout, int options)
{
    Rconnection new_;

    new_ = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new_) error("%s", _("allocation of socket connection failed"));
    new_->connclass = (char *) malloc(strlen("sockconn") + 1);
    if(!new_->connclass) {
	free(new_);
	error("%s", _("allocation of socket connection failed"));
        /* for Solaris 12.5 */ new_ = NULL;
    }
    strcpy(new_->connclass, "sockconn");
    new_->description = (char *) malloc(strlen(host) + 10);
    if(!new_->description) {
	free(new_->connclass); free(new_);
	error("%s", _("allocation of socket connection failed"));
        /* for Solaris 12.5 */ new_ = NULL;
    }
    init_con(new_, host, CE_NATIVE, mode);
    new_->open = &sock_open;
    new_->close = &sock_close;
    new_->vfprintf = &dummy_vfprintf;
    new_->fgetc_internal = &sock_fgetc_internal;
    new_->fgetc = &dummy_fgetc;
    new_->read = &sock_read;
    new_->write = &sock_write;
    new_->connprivate = (void *) malloc(sizeof(struct sockconn));
    if(!new_->connprivate) {
	free(new_->description); free(new_->connclass); free(new_);
	error("%s", _("allocation of socket connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    ((Rsockconn)new_->connprivate)-> port = port;
    ((Rsockconn)new_->connprivate)-> server = server;
    ((Rsockconn)new_->connprivate)-> timeout = timeout;
    ((Rsockconn)new_->connprivate)-> serverfd = serverfd;
    ((Rsockconn)new_->connprivate)-> options = options;
    return new_;
}

Rconnection in_R_newservsock(int port)
{
    Rconnection new_;

    new_ = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new_) error("%s", _("allocation of server socket connection failed"));
    new_->connclass = (char *) malloc(strlen("servsockconn") + 1);
    if(!new_->connclass) {
	free(new_);
	error("%s", _("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ new_ = NULL;
    }
    strcpy(new_->connclass, "servsockconn");
    new_->description = (char *) malloc(strlen("localhost") + 10);
    if(!new_->description) {
	free(new_->connclass); free(new_);
	error("%s", _("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ new_ = NULL;
    }
    init_con(new_, "localhost", CE_NATIVE, "a+");
    new_->close = &servsock_close;
    new_->connprivate = (void *) malloc(sizeof(struct servsockconn));
    if(!new_->connprivate) {
	free(new_->description); free(new_->connclass); free(new_);
	error("%s", _("allocation of server socket connection failed"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
    ((Rservsockconn)new_->connprivate)-> port = port;

    /* socket(), bind(), listen() */
    int sock = R_SockOpen(port); 
    if(sock < 0) {
	free(new_->connprivate); free(new_->description); free(new_->connclass); free(new_);
	error(_("creation of server socket failed: port %d cannot be opened"),
	      port);
	/* for Solaris 12.5 */ new_ = NULL;
    }
#ifdef Unix
    if (sock >= FD_SETSIZE) {
	/* R_SockListen (accept) called from sock_open would fail */
	free(new_->connprivate); free(new_->description); free(new_->connclass); free(new_);
	R_SockClose(sock);
	error("%s", _("file descriptor is too large for select()"));
	/* for Solaris 12.5 */ new_ = NULL;
    }
#endif
    ((Rservsockconn)new_->connprivate)-> fd = sock;
    new_->isopen = TRUE;

    return new_;
}


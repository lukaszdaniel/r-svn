#  File src/library/base/R/unlist.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

unlist <- function(x, recursive=TRUE, use.names=TRUE)
{
    ## for better error messages (islistfactor is not viisble)
    if(is.function(recursive) || length(recursive) != 1L)
        stop("'recursive' must be a length-1 vector")
    if(is.na(recursive)) stop("'recursive' is NA")
    if(.Internal(islistfactor(x, recursive))) {
        URapply <-
            if(recursive) # use rapply()
                 function(x, Fn) .Internal(unlist(rapply(x, Fn, how="list"), recursive, FALSE))
            else function(x, Fn) .Internal(unlist(lapply(x, Fn),             recursive, FALSE))
        lv <- unique(URapply(x, levels))
        nm <- if(use.names) names(.Internal(unlist(x, recursive, use.names)))
        res <- match(URapply(x, as.character), lv)
        ## we cannot make this ordered as level set may have been changed
        structure(res, levels=lv, names=nm, class="factor")
    } else .Internal(unlist(x, recursive, use.names))
}

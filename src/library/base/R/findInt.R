#  File src/library/base/R/findInt.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE,
                         left.open = FALSE, checkSorted = TRUE, checkNA = TRUE)
{
    ## Purpose: returns the indices of  x in vec;  vec[] sorted
    ## ---------------------------------------------------------
    ## Author: Martin Maechler, Date: 4 Jan 2002 (of very different .C version)
    if(checkSorted && !identical(FALSE, is.unsorted(vec)))
	stop("'vec' must be sorted non-decreasingly and not contain NAs")
    .Internal(findInterval(as.double(vec), as.double(x),
                           rightmost.closed, all.inside, left.open, checkNA))
}

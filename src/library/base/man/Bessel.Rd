% File src/library/base/man/Bessel.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{Bessel}
\title{Bessel Functions}
\alias{bessel}
\alias{Bessel}
\alias{besselI}
\alias{besselJ}
\alias{besselK}
\alias{besselY}
\usage{
besselI(x, nu, expon.scaled = FALSE)
besselK(x, nu, expon.scaled = FALSE)
besselJ(x, nu)
besselY(x, nu)
}
\description{
  Bessel Functions of integer and fractional order, of first
  and second kind, \eqn{J_{\nu}}{J(nu)} and \eqn{Y_{\nu}}{Y(nu)}, and
  Modified Bessel functions (of first and third kind),
  \eqn{I_{\nu}}{I(nu)} and \eqn{K_{\nu}}{K(nu)}.
}
\arguments{
  \item{x}{numeric, \eqn{\ge 0}.}

  \item{nu}{numeric; the \emph{order} (maybe fractional and negative) of
    the corresponding Bessel function.}

  \item{expon.scaled}{logical; if \code{TRUE}, the results are
    exponentially scaled in order to avoid overflow
    (\eqn{I_{\nu}}{I(nu)}) or underflow (\eqn{K_{\nu}}{K(nu)}),
    respectively.}
}
\value{
  Numeric vector with the (scaled, if \code{expon.scaled = TRUE})
  values of the corresponding Bessel function.

  The length of the result is the maximum of the lengths of the
  parameters.  All parameters are recycled to that length.
}
\details{
  If \code{expon.scaled = TRUE}, \eqn{e^{-x} I_{\nu}(x)}{exp(-x) I(x;nu)},
  or \eqn{e^{x} K_{\nu}(x)}{exp(x) K(x;nu)} are returned.

  For \eqn{\nu < 0}{nu < 0}, formulae 9.1.2 and 9.6.2 from
  \bibcite{Abramowitz & Stegun}
  are applied (which is probably suboptimal), except for
  \code{besselK} which is symmetric in \code{nu}.

  The current algorithms will give warnings about accuracy loss for
  large arguments.  In some cases, these warnings are exaggerated, and
  the precision is perfect.  For large \code{nu}, say in the order of
  millions, the current algorithms are rarely useful.
}
\source{
  The C code is a translation of Fortran routines from
  \url{https://netlib.org/specfun/ribesl}, \samp{../rjbesl}, etc.
  The four source code files for bessel[IJKY] each contain a paragraph
  \dQuote{Acknowledgement} and \dQuote{References}, a short summary of
  which is
  \describe{
    \item{\code{besselI}}{based on (code by) David J. Sookne, see Sookne (1973)\dots
      Modifications\dots An earlier version was published in Cody (1983).}
    \item{\code{besselJ}}{as \code{besselI}}
    \item{\code{besselK}}{based on (code by) J. B. Campbell (1980)\dots Modifications\dots}
    \item{\code{besselY}}{draws heavily on Temme's Algol program for
      \eqn{Y}\dots and on Campbell's programs for \eqn{Y_\nu(x)}
      \dots. \dots heavily modified.}
  }
}
\references{
  Abramowitz, M. and Stegun, I. A. (1972).
  \emph{Handbook of Mathematical Functions}.
  Dover, New York;
  Chapter 9: Bessel Functions of Integer Order.

  In order of \dQuote{Source} citation above:

  Sookne, David J. (1973).
  Bessel Functions of Real Argument and Integer Order.
  \emph{Journal of Research of the National Bureau of Standards},
  \bold{77B}, 125--132.
  \doi{10.6028/jres.077B.012}.

  Cody, William J. (1983).
  Algorithm 597: Sequence of modified Bessel functions of the first kind.
  \emph{ACM Transactions on Mathematical Software}, \bold{9}(2), 242--245.
  \doi{10.1145/357456.357462}.

  Campbell, J.B. (1980).
  On Temme's algorithm for the modified Bessel function of the third kind.
  \emph{ACM Transactions on Mathematical Software}, \bold{6}(4), 581--586.
  \doi{10.1145/355921.355928}.

  Campbell, J.B. (1979).
  Bessel functions J_nu(x) and Y_nu(x) of float order and float argument.
  \emph{Computer Physics Communications}, \bold{18}, 133--142.
  \doi{10.1016/0010-4655(79)90030-4}.

  Temme, Nico M. (1976).
  On the numerical evaluation of the ordinary Bessel function of the
  second kind.
  \emph{Journal of Computational Physics}, \bold{21}, 343--350.
  \doi{10.1016/0021-9991(76)90032-2}.
}
\seealso{
  Other special mathematical functions, such as
  \ifelse{text}{\code{gamma}}{\link[=gamma]{\eqn{\Gamma(x)}}} and
  \ifelse{text}{\code{beta}}{\link[=beta]{\eqn{B(a,b)}}}.
}
\author{
  Original Fortran code:
  W. J. Cody, Argonne National Laboratory \cr
  Translation to C and adaptation to \R:
  Martin Maechler \email{maechler@stat.math.ethz.ch}.
}
\examples{
require(graphics)

nus <- c(0:5, 10, 20)

x <- seq(0, 4, length.out = 501)
plot(x, x, ylim = c(0, 6), ylab = "", type = "n",
     main = "Bessel Functions  I_nu(x)")
for(nu in nus) lines(x, besselI(x, nu = nu), col = nu + 2)
legend(0, 6, legend = paste("nu=", nus), col = nus + 2, lwd = 1)

x <- seq(0, 40, length.out = 801); yl <- c(-.5, 1)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  J_nu(x)")
abline(h=0, v=0, lty=3)
for(nu in nus) lines(x, besselJ(x, nu = nu), col = nu + 2)
legend("topright", legend = paste("nu=", nus), col = nus + 2, lwd = 1, bty="n")

## Negative nu's --------------------------------------------------
xx <- 2:7
nu <- seq(-10, 9, length.out = 2001)
## --- I() --- --- --- ---
matplot(nu, t(outer(xx, nu, besselI)), type = "l", ylim = c(-50, 200),
        main = expression(paste("Bessel ", I[nu](x), " for fixed ", x,
                                ",  as ", f(nu))),
        xlab = expression(nu))
abline(v = 0, col = "light gray", lty = 3)
legend(5, 200, legend = paste("x=", xx), col=seq(xx), lty=1:5)

## --- J() --- --- --- ---
bJ <- t(outer(xx, nu, besselJ))
matplot(nu, bJ, type = "l", ylim = c(-500, 200),
        xlab = quote(nu), ylab = quote(J[nu](x)),
        main = expression(paste("Bessel ", J[nu](x), " for fixed ", x)))
abline(v = 0, col = "light gray", lty = 3)
legend("topright", legend = paste("x=", xx), col=seq(xx), lty=1:5)

## ZOOM into right part:
matplot(nu[nu > -2], bJ[nu > -2,], type = "l",
        xlab = quote(nu), ylab = quote(J[nu](x)),
        main = expression(paste("Bessel ", J[nu](x), " for fixed ", x)))
abline(h=0, v = 0, col = "gray60", lty = 3)
legend("topright", legend = paste("x=", xx), col=seq(xx), lty=1:5)


##---------------  x --> 0  -----------------------------
x0 <- 2^seq(-16, 5, length.out=256)
plot(range(x0), c(1e-40, 1), log = "xy", xlab = "x", ylab = "", type = "n",
     main = "Bessel Functions  J_nu(x)  near 0\n log - log  scale") ; axis(2, at=1)
for(nu in sort(c(nus, nus+0.5)))
    lines(x0, besselJ(x0, nu = nu), col = nu + 2, lty= 1+ (nu\%\%1 > 0))
legend("right", legend = paste("nu=", paste(nus, nus+0.5, sep=", ")),
       col = nus + 2, lwd = 1, bty="n")

x0 <- 2^seq(-10, 8, length.out=256)
plot(range(x0), 10^c(-100, 80), log = "xy", xlab = "x", ylab = "", type = "n",
     main = "Bessel Functions  K_nu(x)  near 0\n log - log  scale") ; axis(2, at=1)
for(nu in sort(c(nus, nus+0.5)))
    lines(x0, besselK(x0, nu = nu), col = nu + 2, lty= 1+ (nu\%\%1 > 0))
legend("topright", legend = paste("nu=", paste(nus, nus + 0.5, sep = ", ")),
       col = nus + 2, lwd = 1, bty="n")

x <- x[x > 0]
plot(x, x, ylim = c(1e-18, 1e11), log = "y", ylab = "", type = "n",
     main = "Bessel Functions  K_nu(x)"); axis(2, at=1)
for(nu in nus) lines(x, besselK(x, nu = nu), col = nu + 2)
legend(0, 1e-5, legend=paste("nu=", nus), col = nus + 2, lwd = 1)

yl <- c(-1.6, .6)
plot(x, x, ylim = yl, ylab = "", type = "n",
     main = "Bessel Functions  Y_nu(x)")
for(nu in nus){
    xx <- x[x > .6*nu]
    lines(xx, besselY(xx, nu=nu), col = nu+2)
}
legend(25, -.5, legend = paste("nu=", nus), col = nus+2, lwd = 1)

## negative nu in bessel_Y -- was bogus for a long time
curve(besselY(x, -0.1), 0, 10, ylim = c(-3,1), ylab = "")
for(nu in c(seq(-0.2, -2, by = -0.1)))
  curve(besselY(x, nu), add = TRUE)
title(expression(besselY(x, nu) * "   " *
                 {nu == list(-0.1, -0.2, ..., -2)}))
}
\keyword{math}


# Miscellaneous tests

## 'break' in Promise:

quint <- function(arg) {
    sum <- 0
    for (i in 1:5) {
        sum <- sum + 1
	if (i == 3) sum <- sum + arg;
    }
    sum
}

quint(0)
quint(2)
try(quint(break))

## Missing ...

missdots <- function(...) missing(...)
missdots()
missdots(2)

## ALTREP support

#x <- 1:1e10
#stopifnot(length(x) == 1e+10)

#system.time(for (i in 1:1e9) break)

#tools::assertError(y <- x + 1L)

x <- rnorm(1e5)
y <- x + rnorm(1e5)
system.time(lm(y ~ x))

x <- structure(numeric(1e3), class = "foo")
system.time(base::unclass(x))


x <- rnorm(1e7)
system.time(y <- sort(x))
system.time(sort(y))
system.time(anyNA(y))


indx <- seq_along(x)
system.time(anyNA(indx))
system.time(sort(indx))
system.time(sum(as.double(indx)))

x <- 1:1e4
length(x)
head(x)
set.seed(17)
sample(x, 10)
tail(x)

## Reference counting

m <- matrix(1:4, 2)
eval(compiler::compile(quote(m[1,1])))
.Internal(named(m))
.Internal(refcnt(m))
#stopifnot(max(.Internal(named(m)), .Internal(refcnt(m))) == 1)
x <- 1:10
.Internal(refcnt(x))
.Internal(named(x))
stopifnot(max(.Internal(named(x)), .Internal(refcnt(x))) == 65535)

e <- baseenv()
identical(names(e), ls(e, all.names=TRUE, sorted=FALSE))

e <- globalenv()
identical(names(e), names(as.list(e, all.names=TRUE)))


for(e in c(baseenv(), globalenv()))
stopifnot(identical(names(e), ls(e, all.names=TRUE, sorted=FALSE)),
          identical(names(e), names(as.list(e, all.names=TRUE))))

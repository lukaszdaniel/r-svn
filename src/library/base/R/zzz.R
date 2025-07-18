#  File src/library/base/R/zzz.R
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

## top-level assignments that need to be copied to baseloader.R
as.numeric <- as.double
is.name <- is.symbol


## extracted from existing NAMESPACE files in Dec 2003
.knownS3Generics <- local({

    ## include the S3 group generics here
    baseGenerics <- c("Math", "Ops", "Summary", "Complex", "matrixOps",
        "as.character", "as.data.frame", "as.environment", "as.matrix", "as.vector",
        "cbind", "labels", "print", "rbind", "rep", "seq", "seq.int",
        "plot", "sequence", "solve", "summary", "t")

    utilsGenerics <- c("edit", "str")

    graphicsGenerics <- c("contour", "hist", "identify", "image",
        ## "plot",
        "lines", "pairs", "points", "text")

    statsGenerics <- c("add1", "AIC", "anova", "biplot", "coef",
        "confint", "deviance", "df.residual", "drop1", "extractAIC",
        "fitted", "formula", "logLik", "model.frame", "model.matrix",
        "predict", "profile", "qqnorm", "residuals", "se.contrast",
        "terms", "update", "vcov")

    tmp <- rep.int(c("base", "utils", "graphics", "stats"),
                   c(length(baseGenerics), length(utilsGenerics),
                     length(graphicsGenerics), length(statsGenerics)))
    names(tmp) <-
        c(baseGenerics, utilsGenerics, graphicsGenerics, statsGenerics)
    tmp
})

###--- Arguments (for printing and QC analysis) for the .Primitive functions ----

## 1) .ArgsEnv : The non-generics .Primitives :

.ArgsEnv <- new.env(hash = TRUE, parent = emptyenv())

assign("::", function(pkg, name) NULL, envir = .ArgsEnv)
assign(":::", function(pkg, name) NULL, envir = .ArgsEnv)
assign("...length", function() NULL, envir = .ArgsEnv)
assign("...names",  function() NULL, envir = .ArgsEnv)
assign("...elt", function(n) NULL, envir = .ArgsEnv)
assign(".C", function(.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE,
                      ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Fortran",
       function(.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING) NULL,
       envir = .ArgsEnv)
assign(".Call", function(.NAME, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".Call.graphics", function(.NAME, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External", function(.NAME, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External2", function(.NAME, ..., PACKAGE) NULL, envir = .ArgsEnv)
assign(".External.graphics", function(.NAME, ..., PACKAGE) NULL,
       envir = .ArgsEnv)
assign(".Internal", function(call) NULL, envir = .ArgsEnv)
assign(".Primitive", function(name) NULL, envir = .ArgsEnv)
assign(".class2", function(x) NULL, envir = .ArgsEnv)
assign(".isMethodsDispatchOn", function(onOff = NULL) NULL, envir = .ArgsEnv)
assign(".primTrace", function(obj) NULL, envir = .ArgsEnv)
assign(".primUntrace", function(obj) NULL, envir = .ArgsEnv)
assign(".subset", function(x, ...) NULL, envir = .ArgsEnv)
assign(".subset2", function(x, ...) NULL, envir = .ArgsEnv)
assign("UseMethod", function(generic, object) NULL, envir = .ArgsEnv)
assign("attr", function(x, which, exact = FALSE) NULL, envir = .ArgsEnv)
assign("attr<-", function(x, which, value) NULL, envir = .ArgsEnv)
assign("attributes", function(x) NULL, envir = .ArgsEnv)
assign("attributes<-", function(x, value) NULL, envir = .ArgsEnv)
assign("baseenv", function() NULL, envir = .ArgsEnv)
assign("browser",
       function(text="", condition=NULL, expr = TRUE, skipCalls = 0L) NULL,
       envir = .ArgsEnv)
assign("call", function(name, ...) NULL, envir = .ArgsEnv)
assign("class", function(x) NULL, envir = .ArgsEnv)
assign("class<-", function(x, value) NULL, envir = .ArgsEnv)
assign(".cache_class", function(class, extends) NULL, envir = .ArgsEnv)
assign("declare", function(...) NULL, envir = .ArgsEnv)
assign("emptyenv", function() NULL, envir = .ArgsEnv)
assign("enc2native", function(x) NULL, envir = .ArgsEnv)
assign("enc2utf8", function(x) NULL, envir = .ArgsEnv)
assign("environment<-", function(fun, value) NULL, envir = .ArgsEnv)
assign("expression", function(...) NULL, envir = .ArgsEnv)
assign("forceAndCall", function(n, FUN, ...) NULL, envir = .ArgsEnv)
assign("gc.time", function(on = TRUE) NULL, envir = .ArgsEnv)
assign("globalenv", function() NULL, envir = .ArgsEnv)
assign("interactive", function() NULL, envir = .ArgsEnv)
assign("invisible", function(x = NULL) NULL, envir = .ArgsEnv)
assign("is.atomic", function(x) NULL, envir = .ArgsEnv)
assign("is.call", function(x) NULL, envir = .ArgsEnv)
assign("is.character", function(x) NULL, envir = .ArgsEnv)
assign("is.complex", function(x) NULL, envir = .ArgsEnv)
assign("is.double", function(x) NULL, envir = .ArgsEnv)
assign("is.environment", function(x) NULL, envir = .ArgsEnv)
assign("is.expression", function(x) NULL, envir = .ArgsEnv)
assign("is.function", function(x) NULL, envir = .ArgsEnv)
assign("is.integer", function(x) NULL, envir = .ArgsEnv)
assign("is.language", function(x) NULL, envir = .ArgsEnv)
assign("is.list", function(x) NULL, envir = .ArgsEnv)
assign("is.logical", function(x) NULL, envir = .ArgsEnv)
assign("is.name", function(x) NULL, envir = .ArgsEnv)
assign("is.null", function(x) NULL, envir = .ArgsEnv)
assign("is.object", function(x) NULL, envir = .ArgsEnv)
assign("is.pairlist", function(x) NULL, envir = .ArgsEnv)
assign("is.raw", function(x) NULL, envir = .ArgsEnv)
assign("is.recursive", function(x) NULL, envir = .ArgsEnv)
assign("is.single", function(x) NULL, envir = .ArgsEnv)
assign("is.symbol", function(x) NULL, envir = .ArgsEnv)
assign("isS4", function(object) NULL, envir = .ArgsEnv)
assign("list", function(...) NULL, envir = .ArgsEnv)
assign("lazyLoadDBfetch", function(key, file, compressed, hook) NULL,
       envir = .ArgsEnv)
assign("missing", function(x) NULL, envir = .ArgsEnv)
assign("nargs", function() NULL, envir = .ArgsEnv)
assign("nzchar", function(x, keepNA=FALSE) NULL, envir = .ArgsEnv)
assign("oldClass", function(x) NULL, envir = .ArgsEnv)
assign("oldClass<-", function(x, value) NULL, envir = .ArgsEnv)
assign("on.exit", function(expr = NULL, add = FALSE, after = TRUE) NULL, envir = .ArgsEnv)
assign("pos.to.env", function(x) NULL, envir = .ArgsEnv)
assign("proc.time", function() NULL, envir = .ArgsEnv)
assign("quote", function(expr) NULL, envir = .ArgsEnv)
assign("retracemem", function(x, previous = NULL) NULL, envir = .ArgsEnv)
assign("seq_along", function(along.with) NULL, envir = .ArgsEnv)
assign("seq_len", function(length.out) NULL, envir = .ArgsEnv)
assign("standardGeneric", function(f, fdef) NULL, envir = .ArgsEnv)
assign("storage.mode<-", function(x, value) NULL, envir = .ArgsEnv)
assign("substitute", function(expr, env) NULL, envir = .ArgsEnv)
assign("switch", function(EXPR, ...) NULL, envir = .ArgsEnv)
assign("tracemem", function(x) NULL, envir = .ArgsEnv)
assign("unCfillPOSIXlt", function(x) NULL, envir = .ArgsEnv)
assign("unclass", function(x) NULL, envir = .ArgsEnv)
assign("untracemem", function(x) NULL, envir = .ArgsEnv)

assign("Exec", function(expr, envir) NULL, envir = .ArgsEnv)
assign("Tailcall", function(FUN, ...) NULL, envir = .ArgsEnv)


## 2) .GenericArgsEnv : The generic .Primitives :

.S3PrimitiveGenerics <-
    ## not group generics, *nor* assign/extract ops
    ##			"[", "[[", "$", "@", "[<-", "[[<-", "$<-", "@<-"
  c("anyNA", "as.character", "as.complex", "as.double",
    "as.environment", "as.integer", "as.logical", "as.call",
    "as.numeric", "as.raw",
    "c", "dim", "dim<-", "dimnames", "dimnames<-",
    "is.array", "is.finite",
    "is.infinite", "is.matrix", "is.na", "is.nan", "is.numeric",
    "length", "length<-", "levels<-",
    "log2", "log10",
    "names", "names<-", "rep",
    "seq.int", "xtfrm")

.GenericArgsEnv <- local({
    env <- new.env(hash = TRUE, parent = emptyenv())
    ## those with different arglists are overridden below
    for(f in .S3PrimitiveGenerics) {
        fx <- function(x) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    ## now add the group generics
    ## round, signif, log, trunc are handled below
    fx <- function(x) {}
    for(f in c("abs", "sign", "sqrt", "floor", "ceiling",
               "exp", "expm1", "log1p", "log10", "log2",
               "cos", "sin", "tan", "acos", "asin", "atan", "cosh", "sinh",
               "tanh", "acosh", "asinh", "atanh",
	       "cospi", "sinpi", "tanpi",
               "gamma", "lgamma", "digamma", "trigamma",
               "cumsum", "cumprod", "cummax", "cummin")) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    ## ! is unary and handled below
    fx <- function(e1, e2) {}
    for(f in c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|",
               "==", "!=", "<", "<=", ">=", ">")) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    ## "matrixOps"
    fx <- function(x, y) {}
    f <- "%*%"
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    fx <- function(x, y = NULL, ...) {} # e.g {Matrix} has extra arg.
    for(f in c("crossprod", "tcrossprod")) {
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    for(f in c("all", "any", "sum", "prod", "max", "min", "range")) {
        fx <- function(..., na.rm = FALSE) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }

    for(f in c("Arg", "Conj", "Im", "Mod", "Re")) {
        fx <- function(z) {}
        body(fx) <- substitute(UseMethod(ff), list(ff=f))
        environment(fx) <- .BaseNamespaceEnv
        assign(f, fx, envir = env)
    }
    fx <- function(x, recursive = FALSE) UseMethod("anyNA")
    environment(fx) <- .BaseNamespaceEnv
    assign("anyNA", fx, envir = env)
    env
})
### do these outside to get the base namespace as the environment.
assign("!", function(x) UseMethod("!"), envir = .GenericArgsEnv)
assign("as.character", function(x, ...) UseMethod("as.character"),
       envir = .GenericArgsEnv)
assign("as.complex", function(x, ...) UseMethod("as.complex"),
       envir = .GenericArgsEnv)
assign("as.double", function(x, ...) UseMethod("as.double"),
       envir = .GenericArgsEnv)
assign("as.integer", function(x, ...) UseMethod("as.integer"),
       envir = .GenericArgsEnv)
assign("as.logical", function(x, ...) UseMethod("as.logical"),
       envir = .GenericArgsEnv)
#assign("as.raw", function(x) UseMethod("as.raw"), envir = .GenericArgsEnv)
## Conceptually, this is the argument list of  *default* method, not the generic :
## assign("c", function(..., recursive = FALSE, use.names = TRUE) UseMethod("c"),
assign("c", function(...) UseMethod("c"),
       envir = .GenericArgsEnv)
#assign("dimnames", function(x) UseMethod("dimnames"), envir = .GenericArgsEnv)
assign("dim<-", function(x, value) UseMethod("dim<-"), envir = .GenericArgsEnv)
assign("dimnames<-", function(x, value) UseMethod("dimnames<-"),
       envir = .GenericArgsEnv)
assign("length<-", function(x, value) UseMethod("length<-"),
       envir = .GenericArgsEnv)
assign("levels<-", function(x, value) UseMethod("levels<-"),
       envir = .GenericArgsEnv)
assign("log", function(x, base=exp(1)) UseMethod("log"),
       envir = .GenericArgsEnv)
assign("names<-", function(x, value) UseMethod("names<-"),
       envir = .GenericArgsEnv)
assign("rep", function(x, ...) UseMethod("rep"), envir = .GenericArgsEnv)
assign("round", function(x, digits = 0, ...) UseMethod("round"),
       envir = .GenericArgsEnv)
assign("seq.int", function(from, to, by, length.out, along.with, ...)
       UseMethod("seq.int"), envir = .GenericArgsEnv)
assign("signif", function(x, digits=6) UseMethod("signif"),
       envir = .GenericArgsEnv)
assign("trunc", function(x, ...) UseMethod("trunc"), envir = .GenericArgsEnv)
#assign("xtfrm", function(x) UseMethod("xtfrm"), envir = .GenericArgsEnv)

## make this the same object as as.double
assign("as.numeric", .GenericArgsEnv$as.double, envir = .GenericArgsEnv)

## Keep this in sync with ../../tools/R/utils.R
##   tools:::.make_S3_methods_table_for_base()
## for computing the methods table and
##   tools:::.deparse_S3_methods_table_for_base()
## for obtaining the representation used.
## The sorting is "special" (LC_COLLATE = "C", but more), use :
.S3_methods_table <- ## ==  Sys.setlocale("LC_COLLATE","C"); writeLines(tools:::.deparse_S3_methods_table_for_base())
matrix(c("!", "hexmode",
         "!", "octmode",
         "$", "DLLInfo",
         "$", "package_version",
         "$<-", "POSIXlt",
         "$<-", "data.frame",
         "&", "hexmode",
         "&", "octmode",
         "*", "difftime",
         "+", "Date",
         "+", "POSIXt",
         "-", "Date",
         "-", "POSIXt",
         "/", "difftime",
         "[", "AsIs",
         "[", "DLLInfoList",
         "[", "Date",
         "[", "Dlist",
         "[", "POSIXct",
         "[", "POSIXlt",
         "[", "data.frame",
         "[", "difftime",
         "[", "factor",
         "[", "hexmode",
         "[", "listof",
         "[", "noquote",
         "[", "numeric_version",
         "[", "octmode",
         "[", "simple.list",
         "[", "table",
         "[", "warnings",
         "[<-", "Date",
         "[<-", "POSIXct",
         "[<-", "POSIXlt",
         "[<-", "data.frame",
         "[<-", "difftime",
         "[<-", "factor",
         "[<-", "numeric_version",
         "[[", "Date",
         "[[", "POSIXct",
         "[[", "POSIXlt",
         "[[", "data.frame",
         "[[", "factor",
         "[[", "numeric_version",
         "[[<-", "POSIXlt",
         "[[<-", "data.frame",
         "[[<-", "factor",
         "[[<-", "numeric_version",
         "|", "hexmode",
         "|", "octmode",
         "Math", "Date",
         "Math", "POSIXt",
         "Math", "data.frame",
         "Math", "difftime",
         "Math", "factor",
         "Ops", "Date",
         "Ops", "POSIXt",
         "Ops", "data.frame",
         "Ops", "difftime",
         "Ops", "factor",
         "Ops", "numeric_version",
         "Ops", "ordered",
         "Summary", "Date",
         "Summary", "POSIXct",
         "Summary", "POSIXlt",
         "Summary", "data.frame",
         "Summary", "difftime",
         "Summary", "factor",
         "Summary", "numeric_version",
         "Summary", "ordered",
         "all.equal", "POSIXt",
         "all.equal", "character",
         "all.equal", "default",
         "all.equal", "envRefClass",
         "all.equal", "environment",
         "all.equal", "factor",
         "all.equal", "formula",
         "all.equal", "function",
         "all.equal", "language",
         "all.equal", "list",
         "all.equal", "numeric",
         "all.equal", "raw",
         "anyDuplicated", "array",
         "anyDuplicated", "data.frame",
         "anyDuplicated", "default",
         "anyDuplicated", "matrix",
         "anyNA", "POSIXlt",
         "anyNA", "data.frame",
         "anyNA", "numeric_version",
         "aperm", "default",
         "aperm", "table",
         "as.Date", "POSIXct",
         "as.Date", "POSIXlt",
         "as.Date", "character",
         "as.Date", "default",
         "as.Date", "factor",
         "as.Date", "numeric",
         "as.POSIXct", "Date",
         "as.POSIXct", "POSIXlt",
         "as.POSIXct", "default",
         "as.POSIXct", "numeric",
         "as.POSIXlt", "Date",
         "as.POSIXlt", "POSIXct",
         "as.POSIXlt", "character",
         "as.POSIXlt", "default",
         "as.POSIXlt", "factor",
         "as.POSIXlt", "numeric",
         "as.array", "default",
         "as.character", "Date",
         "as.character", "POSIXt",
         "as.character", "condition",
         "as.character", "default",
         "as.character", "error",
         "as.character", "factor",
         "as.character", "hexmode",
         "as.character", "numeric_version",
         "as.character", "octmode",
         "as.character", "srcref",
         "as.data.frame", "AsIs",
         "as.data.frame", "Date",
         "as.data.frame", "POSIXct",
         "as.data.frame", "POSIXlt",
         "as.data.frame", "array",
         "as.data.frame", "character",
         "as.data.frame", "complex",
         "as.data.frame", "data.frame",
         "as.data.frame", "default",
         "as.data.frame", "difftime",
         "as.data.frame", "factor",
         "as.data.frame", "integer",
         "as.data.frame", "list",
         "as.data.frame", "logical",
         "as.data.frame", "matrix",
         "as.data.frame", "model.matrix",
         "as.data.frame", "noquote",
         "as.data.frame", "numeric",
         "as.data.frame", "numeric_version",
         "as.data.frame", "ordered",
         "as.data.frame", "raw",
         "as.data.frame", "table",
         "as.data.frame", "ts",
         "as.double", "POSIXlt",
         "as.double", "difftime",
         "as.expression", "default",
         "as.function", "default",
         "as.list", "Date",
         "as.list", "POSIXct",
         "as.list", "POSIXlt",
         "as.list", "data.frame",
         "as.list", "default",
         "as.list", "difftime",
         "as.list", "environment",
         "as.list", "factor",
         "as.list", "function",
         "as.list", "numeric_version",
         "as.logical", "factor",
         "as.matrix", "POSIXlt",
         "as.matrix", "data.frame",
         "as.matrix", "default",
         "as.matrix", "noquote",
         "as.null", "default",
         "as.single", "default",
         "as.table", "default",
         "as.vector", "POSIXlt",
         "as.vector", "data.frame",
         "as.vector", "factor",
         "by", "data.frame",
         "by", "default",
         "c", "Date",
         "c", "POSIXct",
         "c", "POSIXlt",
         "c", "difftime",
         "c", "factor",
         "c", "noquote",
         "c", "numeric_version",
         "c", "warnings",
         "cbind", "data.frame",
         "chol", "default",
         "chooseOpsMethod", "default",
         "close", "connection",
         "close", "srcfile",
         "close", "srcfilealias",
         "conditionCall", "condition",
         "conditionMessage", "condition",
         "cut", "Date",
         "cut", "POSIXt",
         "cut", "default",
         "determinant", "matrix",
         "diff", "Date",
         "diff", "POSIXt",
         "diff", "default",
         "diff", "difftime",
         "dim", "data.frame",
         "dimnames", "data.frame",
         "dimnames<-", "data.frame",
         "droplevels", "data.frame",
         "droplevels", "factor",
         "duplicated", "POSIXlt",
         "duplicated", "array",
         "duplicated", "data.frame",
         "duplicated", "default",
         "duplicated", "matrix",
         "duplicated", "numeric_version",
         "duplicated", "warnings",
         "flush", "connection",
         "format", "AsIs",
         "format", "Date",
         "format", "POSIXct",
         "format", "POSIXlt",
         "format", "data.frame",
         "format", "default",
         "format", "difftime",
         "format", "factor",
         "format", "hexmode",
         "format", "libraryIQR",
         "format", "numeric_version",
         "format", "octmode",
         "format", "packageInfo",
         "format", "summaryDefault",
         "getDLLRegisteredRoutines", "DLLInfo",
         "getDLLRegisteredRoutines", "character",
         "is.finite", "POSIXlt",
         "is.infinite", "POSIXlt",
         "is.na", "POSIXlt",
         "is.na", "data.frame",
         "is.na", "numeric_version",
         "is.na<-", "default",
         "is.na<-", "factor",
         "is.na<-", "numeric_version",
         "is.nan", "POSIXlt",
         "is.numeric", "Date",
         "is.numeric", "POSIXt",
         "is.numeric", "difftime",
         "isSymmetric", "matrix",
         "julian", "Date",
         "julian", "POSIXt",
         "kappa", "default",
         "kappa", "lm",
         "kappa", "qr",
         "labels", "default",
         "length", "POSIXlt",
         "length<-", "Date",
         "length<-", "POSIXct",
         "length<-", "POSIXlt",
         "length<-", "difftime",
         "length<-", "factor",
         "levels", "default",
         "levels<-", "factor",
         "mean", "Date",
         "mean", "POSIXct",
         "mean", "POSIXlt",
         "mean", "default",
         "mean", "difftime",
         "merge", "data.frame",
         "merge", "default",
         "months", "Date",
         "months", "POSIXt",
         "mtfrm", "Date",
         "mtfrm", "POSIXct",
         "mtfrm", "POSIXlt",
         "mtfrm", "default",
         "nameOfClass", "default",
         "names", "POSIXlt",
         "names<-", "POSIXlt",
         "open", "connection",
         "open", "srcfile",
         "open", "srcfilealias",
         "open", "srcfilecopy",
         "pretty", "default",
         "print", "AsIs",
         "print", "DLLInfo",
         "print", "DLLInfoList",
         "print", "DLLRegisteredRoutines",
         "print", "Date",
         "print", "Dlist",
         "print", "NativeRoutineList",
         "print", "POSIXct",
         "print", "POSIXlt",
         "print", "by",
         "print", "condition",
         "print", "connection",
         "print", "data.frame",
         "print", "default",
         "print", "difftime",
         "print", "eigen",
         "print", "factor",
         "print", "function",
         "print", "hexmode",
         "print", "libraryIQR",
         "print", "listof",
         "print", "noquote",
         "print", "numeric_version",
         "print", "octmode",
         "print", "packageInfo",
         "print", "proc_time",
         "print", "restart",
         "print", "rle",
         "print", "simple.list",
         "print", "srcfile",
         "print", "srcref",
         "print", "summary.table",
         "print", "summary.warnings",
         "print", "summaryDefault",
         "print", "table",
         "print", "warnings",
         "qr", "default",
         "quarters", "Date",
         "quarters", "POSIXt",
         "range", "Date",
         "range", "POSIXct",
         "range", "default",
         "rbind", "data.frame",
         "rep", "Date",
         "rep", "POSIXct",
         "rep", "POSIXlt",
         "rep", "difftime",
         "rep", "factor",
         "rep", "numeric_version",
         "rev", "default",
         "round", "Date",
         "round", "POSIXt",
         "row.names", "data.frame",
         "row.names", "default",
         "row.names<-", "data.frame",
         "row.names<-", "default",
         "rowsum", "data.frame",
         "rowsum", "default",
         "scale", "default",
         "seek", "connection",
         "seq", "Date",
         "seq", "POSIXt",
         "seq", "default",
         "sequence", "default",
         "solve", "default",
         "solve", "qr",
         "sort", "POSIXlt",
         "sort", "default",
         "sort_by", "data.frame",
         "sort_by", "default",
         "split", "Date",
         "split", "POSIXct",
         "split", "data.frame",
         "split", "default",
         "split<-", "data.frame",
         "split<-", "default",
         "subset", "data.frame",
         "subset", "default",
         "subset", "matrix",
         "summary", "Date",
         "summary", "POSIXct",
         "summary", "POSIXlt",
         "summary", "connection",
         "summary", "data.frame",
         "summary", "default",
         "summary", "difftime",
         "summary", "factor",
         "summary", "matrix",
         "summary", "proc_time",
         "summary", "srcfile",
         "summary", "srcref",
         "summary", "table",
         "summary", "warnings",
         "t", "data.frame",
         "t", "default",
         "toString", "default",
         "transform", "data.frame",
         "transform", "default",
         "trunc", "Date",
         "trunc", "POSIXt",
         "truncate", "connection",
         "unique", "POSIXlt",
         "unique", "array",
         "unique", "data.frame",
         "unique", "default",
         "unique", "matrix",
         "unique", "numeric_version",
         "unique", "warnings",
         "units", "difftime",
         "units<-", "difftime",
         "weekdays", "Date",
         "weekdays", "POSIXt",
         "with", "default",
         "within", "data.frame",
         "within", "list",
         "xtfrm", "AsIs",
         "xtfrm", "Date",
         "xtfrm", "POSIXct",
         "xtfrm", "POSIXlt",
         "xtfrm", "data.frame",
         "xtfrm", "default",
         "xtfrm", "difftime",
         "xtfrm", "factor",
         "xtfrm", "numeric_version"),
       ncol = 2L, byrow = TRUE,
       dimnames = list(NULL, c("generic", "class")))

## Create .as.data.frame.vector :=  deprecated version  as.data.frame.vector
## which will be used for the now-deprecated as.data.frame.<foo> methods:
local({
    bdy <- body(as.data.frame.vector)
    bdy <- bdy[c(1:2, seq_along(bdy)[-1L])] # taking [(1,2,2:n)] to insert at [2]:
    ## deprecation warning only when not called by method dispatch from as.data.frame():
    bdy[[2L]] <- quote(
        if((sys.nframe() <= 1L ||
	    !(identical(sys.function(-1L), as.data.frame) || ## when as.data.frame is S4 generic:
	      (.isMethodsDispatchOn() &&
	       methods::is(sys.function(-1L), 'derivedDefaultMethod') &&
	       identical(sys.function(-1L)@generic,
			 structure('as.data.frame', package = 'base'))
	       ))))
	.Deprecated(
	    msg = gettextf(
		"Direct call of '%s()' is deprecated.  Use '%s()' or '%s()' instead",
		"PLACEHOLDER", "as.data.frame.vector", "as.data.frame")))
    ii <- c(2L, 3L,2L, 3L)
    stopifnot(identical(bdy[[ii]], "PLACEHOLDER"))
    ASDFV <- function(fn) {## = our deprecated AS.Data.Frame.Vector()
        bdy[[ii]] <- fn  # now basically calling `body(.) <- bdy` :
        as.function(c(as.list(formals(as.data.frame.vector)), list(bdy)), .BaseNamespaceEnv)
    }
    ## now replace all the as.data.frame.<foo> methods which were := as.data.frame.vector :
    for(f in paste0("as.data.frame.",
                    c("raw", "logical", "integer", "numeric", "complex",
                      "factor", "ordered", "Date", "difftime", "POSIXct",
                      "noquote", "numeric_version")))
        assign(f, ASDFV(f), .BaseNamespaceEnv)
})

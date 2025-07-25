### This is the system Rprofile file. It is always run on startup.
### Additional commands can be placed in site or user Rprofile files
### (see ?Rprofile).

### Copyright (C) 1995-2025 The R Core Team

### Notice that it is a bad idea to use this file as a template for
### personal startup files, since things will be executed twice and in
### the wrong environment (user profiles are run in .GlobalEnv).

.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- as.environment(2)
assign(".Autoloaded", NULL, envir = .AutoloadEnv)
T <- TRUE
F <- FALSE
R.version <- structure(R.Version(), class = "simple.list")
version <- R.version            # for S compatibility

## for backwards compatibility only
R.version.string <- R.version$version.string

## NOTA BENE: options() for non-base package functionality are in places like
##            --------- ../utils/R/zzz.R

options(keep.source = interactive())
options(warn = 0)
# options(repos = c(CRAN="@CRAN@"))
# options(BIOC = "http://www.bioconductor.org")

## setting from an env variable added in 4.0.2
local({to <- as.integer(Sys.getenv("R_DEFAULT_INTERNET_TIMEOUT", 60))
    if (is.na(to) || to <= 0) to <- 60L
    options(timeout = to)
})
local({
    if(nzchar(nr <- Sys.getenv("R_DEFAULT_NETRC")))
        options(netrc = nr)
})
options(encoding = "native.enc")
options(show.error.messages = TRUE)
## keep in sync with PrintDefaults() in  ../../main/print.c :
options(scipen = 0)
options(max.print = 99999)# max. #{entries} in internal printMatrix()
options(add.smooth = TRUE)# currently only used in 'plot.lm'

if(!interactive())
    options(showErrorCalls = TRUE)
options(catch.script.errors = FALSE)

local({dp <- Sys.getenv("R_DEFAULT_PACKAGES")
       if(identical(dp, "")) ## it fact methods is done first
           dp <- c("datasets", "utils", "grDevices", "graphics",
                   "stats", "methods")
       else if(identical(dp, "NULL")) dp <- character(0)
       else dp <- strsplit(dp, ",")[[1]]
       dp <- sub("[[:blank:]]*([[:alnum:]]+)", "\\1", dp) # strip whitespace
       options(defaultPackages = dp)
    })

## Expand R_LIBS_* environment variables.
Sys.setenv(R_LIBS_SITE =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
Sys.setenv(R_LIBS_USER =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

local({
    if(nzchar(tl <- Sys.getenv("R_SESSION_TIME_LIMIT_CPU")))
        setSessionTimeLimit(cpu = tl)
    if(nzchar(tl <- Sys.getenv("R_SESSION_TIME_LIMIT_ELAPSED")))
        setSessionTimeLimit(elapsed = tl)
})

.First.sys <- function()
{
    verbose <- getOption("verbose", FALSE)
    for(pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = !verbose, warn.conflicts = FALSE,
                       character.only = TRUE)
        if(!res)
            warning(gettextf("package %s in options(\"defaultPackages\") was not found", sQuote(pkg)),
                    call. = FALSE, domain = NA)
    }
}

## called at C level in the startup process prior to .First.sys
.OptRequireMethods <- function()
{
    pkg <- "methods" # done this way to avoid R CMD check warning
    if(pkg %in% getOption("defaultPackages"))
        if(!require(pkg, quietly = !getOption("verbose", FALSE), warn.conflicts = FALSE,
                    character.only = TRUE))
            warning("package \"methods\" in options(\"defaultPackages\") was not found",
                    call. = FALSE)
}

if(nzchar(Sys.getenv("R_BATCH"))) {
    .Last.sys <- function()
    {
        cat("> proc.time()\n")
        print(proc.time())
    }
    ## avoid passing on to spawned R processes
    ## A system has been reported without Sys.unsetenv, so try this
    try(Sys.setenv(R_BATCH=""))
}

local({
    if(nzchar(rv <- Sys.getenv("_R_RNG_VERSION_")))
        suppressWarnings(RNGversion(rv))
})

.sys.timezone <- NA_character_

local({
    ## create an active binding for .Library.site, so that it can be
    ## modified after the base environment is locked

    ## remove the binding in the lazyload data base 
    .Internal(mkUnbound(as.name(".Library.site")))
    siteLibrary <- character()
    slfun <- function(v) {
        if (!missing(v))
            siteLibrary <<- v
        siteLibrary
    }

    makeActiveBinding(".Library.site", slfun, baseenv())

    ## make .Library.site accessible also from global environment to
    ## preserve functionality of site profiles assigning to it directly
    ## (originally, site profiles were run in base environment)

    makeActiveBinding(".Library.site", slfun, globalenv())
})

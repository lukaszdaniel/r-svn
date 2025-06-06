### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx",
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- graphics::par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               graphics::mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = 0.8, col = "orchid", las = 3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") -
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx", evalq(
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
           if(identical(Sys.getenv("_R_CHECK_SCREEN_DEVICE_"), "stop"))
               options(editor = function(...) utils:::check_screen_device("editor"))
	   delayedAssign("T", stop("T used instead of TRUE", domain = "R-base"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE", domain = "R-base"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
           if(length(newitems)) tools:::detachPackages(newitems)
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning(gettextf("items %s were removed from the search path",
                               paste(sQuote(missitems), collapse=", ")),
                       call. = FALSE, immediate. = TRUE, domain = "R-base")
           ## Old massaged files will not have set .old_wd.
           if(exists(".old_wd") && (wd <- getwd()) != .old_wd) {
               warning(gettextf("working directory was changed to %s, resetting",
                               sQuote(wd)),
                       call. = FALSE, immediate. = TRUE, domain = "R-base")
               setwd(.old_wd)
           }
           ## stop in case users left connections open,
           ## also indicating that parallel cluster are still running
           if(Sys.getenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_", FALSE)){
               sC <- showConnections()
               if(nrow(sC)){
                   stop("connections left open:\n",
                       paste(apply(sC[,1:2, drop = FALSE], 1L, function(x)
                           paste0("\t", x[1L], " (", x[2L], ")")), collapse="\n"),
				       call. = FALSE, domain = NA)
               }
           }
       },
       as.environment("CheckExEnv")), pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## Do this before loading the package,
## since packages have been known to change settings.
## Force a size that is close to on-screen devices, fix paper.
## don't rename par.postscript for back-compatibility of reference output.
grDevices::pdf.options(width = 7, height = 7, paper = "special", reset = TRUE)
grDevices::pdf(paste(pkgname, "-Ex.pdf", sep=""), encoding = "ISOLatin1")

assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))

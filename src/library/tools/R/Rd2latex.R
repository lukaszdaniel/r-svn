#  File src/library/tools/R/Rd2latex.R
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

## TODO: can we do something useful with cross-package links?


### * .Rd_get_latex

# Return latex form of text, encoded in UTF-8.
.Rd_get_latex <-
function(x)
{
    tf <- tempfile()
    save <- options(useFancyQuotes = FALSE)
    on.exit({options(save); unlink(tf)})
    tryCatch(Rd2latex(x, tf, fragment = TRUE, outputEncoding = "UTF-8"),
             error = function(e) return(character()))
    enc2utf8(readLines(tf, warn = FALSE, encoding = "UTF-8"))
}

latex_canonical_encoding  <- function(encoding)
{
    if (encoding == "") encoding <- utils::localeToCharset()[1L]
    encoding <- tolower(encoding)
    encoding <- sub("iso_8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding <- sub("iso8859-([0-9]+)", "iso-8859-\\1", encoding)

    encoding[encoding == "iso-8859-1"] <-  "latin1"
    encoding[encoding == "iso-8859-2"] <-  "latin2"
    encoding[encoding == "iso-8859-3"] <-  "latin3"
    encoding[encoding == "iso-8859-4"] <-  "latin4"
    encoding[encoding == "iso-8859-5"] <-  "cyrillic"
    encoding[encoding == "iso-8859-6"] <-  "arabic"
    encoding[encoding == "iso-8859-7"] <-  "greek"
    encoding[encoding == "iso-8859-8"] <-  "hebrew"
    encoding[encoding == "iso-8859-9"] <-  "latin5"
    encoding[encoding == "iso-8859-10"] <-  "latin6"
    encoding[encoding == "iso-8859-14"] <-  "latin8"
    encoding[encoding %in% c("latin-9", "iso-8859-15")] <-  "latin9"
    encoding[encoding == "iso-8859-16"] <-  "latin10"
    encoding[encoding == "utf-8"] <-  "utf8"
    encoding
}

## 'encoding' is passed to parse_Rd, as the input encoding
Rd2latex <- function(Rd, out = "", defines = .Platform$OS.type,
                     stages = "render",
		     outputEncoding = "UTF-8", fragment = FALSE, ...,
                     writeEncoding = outputEncoding != "UTF-8",
		     concordance = FALSE)
{
    encode_warn <- FALSE
    WriteLines <-
        if(outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
            function(x, con, outputEncoding, ...)
                writeLines(x, con, useBytes = TRUE, ...)
        } else {
            function(x, con, outputEncoding, ...) {
                y <- iconv(x, "UTF-8", outputEncoding,  mark = FALSE)
                if (anyNA(y)) {
                    y <- iconv(x, "UTF-8", outputEncoding,
                               sub = "byte", mark = FALSE)
                    encode_warn <<- TRUE
                }
                writeLines(y, con, useBytes = TRUE, ...)
            }
    }

    conc <- if(concordance) activeConcordance() # else NULL
    
    last_char <- ""
    skipNewline <- FALSE
    of0 <- function(...) of1(paste0(...))
    of1 <- function(text) {
        if (skipNewline) {
            skipNewline <<- FALSE
            if (text == "\n") return()
        }
    	if (concordance)
    	    conc$addToConcordance(text)
        nc <- nchar(text)
        last_char <<- substr(text, nc, nc)
        WriteLines(text, con, outputEncoding, sep = "")
    }

    trim <- function(x) {
        x <- psub1("^\\s*", "", as.character(x))
        psub1("\\s*$", "", x)
    }

    envTitles <- c("\\description"="Description", "\\usage"="Usage",
        "\\arguments"="Arguments",
        "\\format"="Format", "\\details"="Details", "\\note"="Note",
        "\\section"="", "\\author"="Author",
        "\\references"="References", "\\source"="Source",
        "\\seealso"="SeeAlso", "\\examples"="Examples",
        "\\value"="Value")

    sectionExtras <-
    c("\\usage"="verbatim",
      "\\examples"="ExampleCode")

    inCodeBlock <- FALSE ## used to indicate to texify where we are
    inCode <- FALSE
    inEqn <- FALSE
    inPre <- FALSE
    sectionLevel <- 0
    hasFigures <- FALSE

    startByte <- function(x) {
    	srcref <- attr(x, "srcref")
    	if (is.null(srcref)) -1L
    	else srcref[2L]
    }

    addParaBreaks <- function(x, tag) {
        if (isBlankLineRd(x)) "\n"
        else if (startByte(x) == 1L) psub("^\\s+", "", x)
        else x
    }

    texify <- function(x, code = inCodeBlock) {
        if(inEqn) return(x)
        if (!code) {
	    # Need to be careful to handle backslash, so do it in three steps.
	    # First, mark all the ones in the original text, but don't add
	    # any other special chars
	    x <- fsub("\\", "\\bsl", x)
	    # Second, escape other things, introducing more backslashes
	    x <- psub("([&$%_#])", "\\\\\\1", x)
	    ## pretty has braces in text.
	    x <- fsub("{", "\\{", x)
	    x <- fsub("}", "\\}", x)
	    x <- fsub("^", "\\textasciicircum{}", x)
	    x <- fsub("~", "\\textasciitilde{}", x)
	    # Third, add the terminal braces to the backslash
	    x <- fsub("\\bsl", "\\bsl{}", x)
	} else {
	    ## inCodeBlock/inPre is in alltt, where only \ { } have their usual meaning
	    if (inCodeBlock) {
		## We do want to escape { }, but unmatched braces had
		## to be escaped in earlier versions (e.g. Paren.Rd, body.tex).
		## So fix up for now
		x <- fsub1('"\\{"', '"{"', x)
	    } else if (inPre) {
		BSL <- '@BSL@'
		x <- fsub("\\", BSL, x)
		x <- psub("(?<!\\\\)\\{", "\\\\{", x)
		x <- psub("(?<!\\\\)}", "\\\\}", x)
		x <- fsub(BSL, "\\bsl{}", x)
		x <- psub("\\\\\\\\var\\\\\\{([^\\\\]*)\\\\}", "\\\\var{\\1}", x)
	    } else {
		## cat(sprintf("\ntexify in: '%s'\n", x))
		BSL <- '@BSL@'
		x <- fsub("\\", BSL, x)
		x <- psub("(?<!\\\\)\\{", "\\\\{", x)
		x <- psub("(?<!\\\\)}", "\\\\}", x)
		x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
		x <- fsub("^", "\\textasciicircum{}", x)
		x <- fsub("~", "\\textasciitilde{}", x)
		x <- fsub(BSL, "\\bsl{}", x)
		## avoid conversion to guillemets
		x <- fsub("<<", "<{}<", x)
		x <- fsub(">>", ">{}>", x)
		x <- fsub(",,", ",{},", x) # ,, is a ligature in the ec and lmodern fonts.
		## cat(sprintf("\ntexify out: '%s'\n", x))
	    }
	}
        x
    }

    # The quotes were Rd.sty macros, but Latex limitations (e.g. nesting \preformatted within)
    # mean we get better results expanding them here.

    wrappers <- list("\\dQuote" =c("``", "''"),
    		     "\\sQuote" =c("`", "'"),
    		     "\\cite"   =c("\\Cite{", "}"))

    writeWrapped <- function(block, tag) {
    	wrapper <- wrappers[[tag]]
    	if (is.null(wrapper))
    	    wrapper <- c(paste0(tag, "{"), "}")
    	if (concordance)
    	    conc$saveSrcref(block)
    	of1(wrapper[1L])
    	writeContent(block, tag)
    	of1(wrapper[2L])
    }

    writeURL <- function(block, tag) {
        ## really verbatim
        if (tag == "\\url")
            url <- as.character(block)
        else {
            url <- as.character(block[[1L]])
            tag <- "\\Rhref"
        }
        ## cleanup URL
        url <- lines2str(url)
        ## escape special characters for LaTeX
        url <- fsub("\\", "\\\\", url)
        ## support \href inside \tabular
        url <- fsub("%", "\\%",  url)
        url <- fsub("#", "\\#",  url)
        url <- fsub("&", "\\&",  url)  # needs hyperref >= 6.78n (2008-12-26)
        if (concordance)
            conc$saveSrcref(block)
    	of0(tag, "{", url, "}")
        if (tag == "\\Rhref") {
            if (concordance)
                conc$saveSrcref(block[[2L]])	
            of1("{")
            writeContent(block[[2L]], tag)
            of1("}")
        }
    }

    ## Currently ignores [option] except for [=dest] form
    ## (as documented)
    ## FIXME: so should not output cross-package links (unless for refman ...)
    writeLink <- function(tag, block) {
        parts <- get_link(block, tag, Rdfile)
        if (concordance)
            conc$saveSrcref(block)
        if (all(RdTags(block) == "TEXT")) {
            of0("\\LinkA{", latex_escape_name(parts$topic))
        } else { # don't \index link text containing markup etc
            of1("\\LinkB{")
            writeContent(block, tag)
        }
        of0("}{",
            latex_link_trans0(parts$dest), "}")
    }

    writeDR <- function(block, tag) {
    	if (concordance)
    	    conc$saveSrcref(block)
        if (length(block) > 1L) {
            of1('## Not run: ')
            writeContent(block, tag)
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
    }

    ltxstriptitle <- function(x)
    {
        x <- fsub("\\R", "\\R{}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x
    }

    latex_escape_name <- function(x)
    {
        x <- fsub("\\", "\\textbackslash", x)
        x <- psub("([$#_&{}])", "\\\\\\1", x) #- escape them
        x <- fsub("\\textbackslash", "\\textbackslash{}", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("%", "\\Rpercent{}", x)
        ## avoid conversion to guillemets
        x <- fsub("<<", "<{}<", x)
        x <- fsub(">>", ">{}>", x)
        x
    }

    latex_escape_name_for_index <- function(x) {
        ## In the index, ! @ | are escape characters, and one must quote
        ## these characters in the \index command by putting a double
        ## quotation mark (") in front of them, and one can only place a
        ## " in the index by quoting it.
        x <- latex_escape_name(x)
        ## <FIXME>
        ## Should really handle all of the above ...
        x <- fsub("@", "\"@", x)
        ## </FIXME>
        x
    }

    latex_link_trans0 <- function(x)
    {
        x <- fsub("\\Rdash", ".Rdash.", x)
        x <- fsub("-", ".Rdash.", x)
        x <- fsub("\\_", ".Rul.", x)
        x <- fsub("\\$", ".Rdol.", x)
        x <- fsub("\\^", ".Rcaret.", x)
        x <- fsub("^", ".Rcaret.", x)
        x <- fsub("_", ".Rul.", x)
        x <- fsub("$", ".Rdol.", x)
        x <- fsub("\\#", ".Rhash.", x) #
        x <- fsub("#", ".Rhash.", x)   #
        x <- fsub("\\&", ".Ramp.", x)
        x <- fsub("&", ".Ramp.", x)
        x <- fsub("\\~", ".Rtilde.", x)
        x <- fsub("~", ".Rtilde.", x)
        x <- fsub("\\%", ".Rpcent.", x)
        x <- fsub("%", ".Rpcent.", x)
        x <- fsub("\\\\", ".Rbl.", x)
        x <- fsub("{", ".Rlbrace.", x)
        x <- fsub("}", ".Rrbrace.", x)
        x
    }

    latex_code_alias <- function(x)
    {
        x <- fsub("{", "\\{", x)
        x <- fsub("}", "\\}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("<-", "<\\Rdash{}", x)
        x <- psub("([!|])", '"\\1', x)
        x
    }

    currentAlias <- NA_character_

    writeAlias <- function(block, tag) {
        alias <- as.character(block)
        if(length(alias) > 1L)
            stop(gettextf("alias:\n%s\nis not one line",
                 sQuote(paste(alias, collapse = "\n"))))
        alias <- trim(alias)
        aa <- "\\aliasA{"
        ## Some versions of hyperref (from 6.79d) have trouble indexing these
        ## |, || in base, |.bit, %||% in ggplot2 ...
        ## And texindy used by some versions of texi2dvi chokes on {/(
        if(grepl("[|{(]", alias)) aa <- "\\aliasB{"
        if(is.na(currentAlias)) currentAlias <<- name
        if (pmatch(paste0(currentAlias, "."), alias, 0L)) {
            aa <- "\\methaliasA{"
        } else currentAlias <<- alias
        ## 'name' is linked from the header
        if (alias == name) return()
        alias2 <- latex_link_trans0(alias)
        if (concordance)
            conc$saveSrcref(block)
        of0(aa, latex_code_alias(alias), "}{",
            latex_escape_name(name), "}{", alias2, "}\n")
    }

    writeBlock <- function(block, tag, blocktag) {
    	if (concordance)
    	    conc$saveSrcref(block)
	switch(tag,
               UNKNOWN =,
               VERB = of1(texify(block, TRUE)),
               RCODE = of1(texify(block, TRUE)),
               TEXT = of1(addParaBreaks(texify(block), blocktag)),
               USERMACRO =,
               "\\newcommand" =,
               "\\renewcommand" = {},
               COMMENT = if (startByte(block) == 1L ||
                             (!inCodeBlock && last_char == "")) # indented comment line
                             skipNewline <<- TRUE,
               LIST = writeContent(block, tag),
               ## Avoid Rd.sty's \describe, \Enumerate and \Itemize:
               ## They don't support verbatim arguments, which we might need.
               "\\describe"= {
                   of1("\\begin{description}\n")
                   writeContent(block, tag)
                   of1("\n\\end{description}\n")
               },
               "\\enumerate"={
                   of1("\\begin{enumerate}\n")
                   writeContent(block, tag)
                   of1("\n\\end{enumerate}\n")
               },
               "\\itemize"= {
                   of1("\\begin{itemize}\n")
                   writeContent(block, tag)
                   of1("\n\\end{itemize}\n")
               },
               ## Verbatim-like
               "\\command"=,
               "\\env" =,
               "\\kbd"=,
               "\\option" =,
               "\\samp" = writeWrapped(block, tag),
               ## really verbatim
               "\\url"=,
               "\\href"= writeURL(block, tag),
               ## R-like
               "\\code"= {
                   inCode <<- TRUE
                   writeWrapped(block, tag)
                   inCode <<- FALSE
               },
               ## simple wrappers
               "\\abbr" =,
               "\\acronym" =,
               "\\bold"=,
               "\\dfn"=,
               "\\dQuote"=,
               "\\email"=,
               "\\emph"=,
               "\\file" =,
               "\\pkg" =,
               "\\sQuote" =,
               "\\strong"=,
               "\\var" =,
               "\\cite" =
                   if (inCodeBlock) writeContent(block, tag)
                   else writeWrapped(block, tag),
               "\\preformatted"= {
                   inPre <<- TRUE
                   of1("\\begin{alltt}")
                   writeContent(block, tag)
                   of1("\\end{alltt}\n")
                   inPre <<- FALSE
               },
               "\\Sexpr"= { of1("\\begin{verbatim}\n")  # This is only here if processing didn't get it...
	       	            of0(as.character.Rd(block, deparse=TRUE))
	       	            of1("\n\\end{verbatim}\n")
	       	          },

               "\\verb"= {
                   of0("\\AsIs{\\texttt{")
                   writeContent(block, tag)
                   of1("}}")
               },
               "\\special"= writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               "\\cr" = of1("\\\\{}"), ## might be followed by [
               "\\dots" =,
               "\\ldots" = of1(if(inCode || inCodeBlock) "..."  else tag),
               "\\R" = of0(tag, "{}"),
               "\\donttest" =, "\\dontdiff" = writeContent(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = {
                   ## some people put more things in \enc than a word,
                   ## but Rd2txt does not cover that case ....
                   if (outputEncoding == "ASCII")
                       writeContent(block[[2L]], tag)
                   else
                       writeContent(block[[1L]], tag)
               } ,
               "\\eqn" =,
               "\\deqn" = {
               	   if (concordance)
               		conc$saveSrcref(block[[1L]])
                   of0(tag, "{")
                   inEqn <<- TRUE
                   writeContent(block[[1L]], tag)
                   inEqn <<- FALSE
                   of0('}{}')
               },
               "\\figure" = {
               	   if (concordance)
               		conc$saveSrcref(block[[1L]])
               	   of0('\\Figure{')
               	   writeContent(block[[1L]], tag)
               	   of0('}{')
               	   if (length(block) > 1L) {
               	       if (concordance)
               	   	   conc$saveSrcref(block[[2L]])	
		       includeoptions <- .Rd_get_latex(block[[2L]])
                       ## this was wrong if length(includeopptions) > 1
		       if (length(includeoptions))
                           for (z in includeoptions)
                               if(startsWith(z, "options: "))
                                   of0(sub("^options: ", "", z))
                   }
               	   of0('}')
               	   hasFigures <<- TRUE
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" =,
               "\\S4method" = {
                   ## should not get here
               },
               "\\tabular" = writeTabular(block),
               "\\subsection" = writeSection(block, tag),
               "\\if" =,
               "\\ifelse" =
		    if (testRdConditional("latex", block, Rdfile))
               		writeContent(block[[2L]], tag)
               	    else if (tag == "\\ifelse")
               	    	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block)) {
               	   if (concordance)
               		conc$saveSrcref(block[[i]])
		   of1(block[[i]])
		   },
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
        ## FIXME does no check of correct format
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1L || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, gettext("\\tabular format must be simple text"))
        tags <- RdTags(content)
	of0('\n\\Tabular{', format, '}{')
        if (concordance)
            conc$saveSrcref(table[[1L]])
        for (i in seq_along(tags)) {
            if (concordance)
                conc$saveSrcref(content[[i]])	
            switch(tags[i],
                   "\\tab" = of1("&"),
                   "\\cr" = of1("\\\\{}"),
                   writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        of1('}')
    }

    writeContent <- function(blocks, blocktag) {
        inList <- FALSE
        itemskip <- FALSE

	tags <- RdTags(blocks)

	i <- 0
	while (i < length(tags)) {
	    i <- i + 1
            block <- blocks[[i]]
            tag <- attr(block, "Rd_tag")
            ## this should not be null, but it might be in a erroneous Rd file
            if(!is.null(tag))
            switch(tag,
                   "\\method" =,
                   "\\S3method" =,
                   "\\S4method" = {
                   	blocks <- transformMethod(i, blocks, Rdfile)
                   	tags <- RdTags(blocks)
                   	i <- i - 1
                   },
                   "\\item" = {
                       if (blocktag %in% c("\\value", "\\arguments") && !inList) {
                           of1("\\begin{ldescription}\n")
                           inList <- TRUE
                       }
                       switch(blocktag,
                              "\\describe" = {
                              	  if (concordance)
                              	      conc$saveSrcref(block[[1L]])
                                  of1('\\item[')
                                  writeContent(block[[1L]], tag)
                                  of1('] ')
                                  if (concordance)
                                      conc$saveSrcref(block[[2L]])
                                  writeContent(block[[2L]], tag)
                              },
                              "\\value"=,
                              "\\arguments"={
                              	  if (concordance)
                              	      conc$saveSrcref(block[[1L]])
                                  of1('\\item[')
                                  inCode <<- TRUE
                                  writeItemAsCode(tag, block[[1L]])
                                  inCode <<- FALSE
                                  of1('] ')
                                  if (concordance)
                                      conc$saveSrcref(block[[2L]])
                                  writeContent(block[[2L]], tag)
                              },
                              "\\enumerate" =,
                              "\\itemize"= {
                                  of1("\\item{} ")
                                  itemskip <- TRUE
                              })
                       itemskip <- TRUE
                   },
                   "\\cr" = of1("\\\\{}"), ## might be followed by [
               { # default
                   if (inList && tag != "COMMENT"
                              && !(tag == "TEXT" && isBlankRd(block))) {
                       of1("\\end{ldescription}\n")
                       inList <- FALSE
                   }
                   if (itemskip) {
                       ## The next item must be TEXT, and start with a space.
                       itemskip <- FALSE
                       if (tag == "TEXT") {
                           txt <- psub("^ ", "", as.character(block))
                           of1(texify(txt))
                       } else writeBlock(block, tag, blocktag) # should not happen
                   } else writeBlock(block, tag, blocktag)
               })
	}
        if (inList) of1("\\end{ldescription}\n")
    }

    writeSectionInner <- function(section, tag)
    {
        if (length(section)) {
	    ## need \n unless one follows, so
	    nxt <- section[[1L]]
	    if (is.null(nxttag <- attr(nxt, "Rd_tag"))) # erroneous Rd file
		return()
	    if (nxttag %notin% c("TEXT", "RCODE") ||
		!startsWith(as.character(nxt), "\n")) of1("\n")
	    writeContent(section, tag)
	    if (last_char != "\n") of1("\n")
	}
    }

    writeSection <- function(section, tag) {
        if (tag == "\\encoding")
            return()
    	if (concordance)
    	    conc$saveSrcref(section)
        save <- sectionLevel
        sectionLevel <<- sectionLevel + 1
        if (tag == "\\alias")
            writeAlias(section, tag)
        else if (tag == "\\keyword") {
            key <- trim(section)
            if(any(key %in% .Rd_keywords_auto))
                return()
            of0("\\keyword{", latex_escape_name(key), "}{", ltxname, "}\n")
        }
        else if (tag == "\\concept") {
            key <- trim(section)
            of0("\\keyword{", latex_escape_name_for_index(key), "}{", ltxname, "}\n")
        }
        else if (tag == "\\section" || tag == "\\subsection") {
            macro <- c("Section", "SubSection", "SubSubSection")[min(sectionLevel, 3)]
    	    of0("%\n\\begin{", macro, "}{")
            writeContent(section[[1L]], tag)
            of1("}")
    	    writeSectionInner(section[[2L]], tag)
            of0("\\end{", macro, "}\n")
    	} else {
            title <- envTitles[tag]
            of0("%\n\\begin{", title, "}")
            extra <- sectionExtras[tag]
            if(!is.na(extra)) of0("\n\\begin{", extra, "}")
            if(tag %in% c("\\usage", "\\examples")) inCodeBlock <<- TRUE
            writeSectionInner(section, tag)
 	    inCodeBlock <<- FALSE
            if(!is.na(extra)) of0("\\end{", extra, "}\n")
            of0("\\end{", title, "}\n")
        }
        sectionLevel <<- save
    }

    writeItemAsCode <- function(blocktag, block) {
        ## Keep this in rsync with writeItemAsCode() in Rd2HTML.R!

        ## Argh.  Quite a few packages put the items in their value
        ## section inside \code.
        for(i in which(RdTags(block) == "\\code"))
            attr(block[[i]], "Rd_tag") <- "Rd"

        s <- as.character.Rd(block)
        s[s %in% c("\\dots", "\\ldots")] <- "..."
        s <- trimws(strsplit(paste(s, collapse = ""), ",", fixed = TRUE)[[1]])
        s <- s[nzchar(s)]
        s <- sprintf("\\code{%s}", texify(s))
        s <- paste0(s, collapse = ", ")
        of1(s)
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, fragment=fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

   if (outputEncoding != "ASCII" && !fragment) {
        latexEncoding <- latex_canonical_encoding(outputEncoding)
        if(writeEncoding) of0("\\inputencoding{", latexEncoding, "}\n")
    } else latexEncoding <- NA

    if (fragment) {
    	if (sections[1L] %in% names(sectionOrder))
    	    for (i in seq_along(sections))
    	    	writeSection(Rd[[i]], sections[i])
    	else
    	    for (i in seq_along(sections))
    	    	writeBlock(Rd[[i]], sections[i], "")
    } else {
	## we know this has been ordered by prepare2_Rd, but
	## need to sort the aliases (if any)
	nm <- character(length(Rd))
	isAlias <- sections == "\\alias"
	sortorder <- if (any(isAlias)) {
	    nm[isAlias] <- sapply(Rd[isAlias], as.character)
	    order(sectionOrder[sections], toupper(nm), nm)
	} else  order(sectionOrder[sections])
	Rd <- Rd[sortorder]
	sections <- sections[sortorder]

	title <- .Rd_get_latex(.Rd_get_section(Rd, "title"))
        ## This might have blank lines
        title <- paste(title[nzchar(title)], collapse = " ")

	name <- Rd[[2L]]
	if (concordance)
	    conc$saveSrcref(name)
	name <- trim(as.character(Rd[[2L]][[1L]]))
	ltxname <- latex_escape_name(name)

	of0('\\HeaderA{', ltxname, '}{',
	    ltxstriptitle(title), '}{',
	    latex_link_trans0(name), '}\n')

	for (i in seq_along(sections)[-(1:2)])
	    writeSection(Rd[[i]], sections[i])
    }
    if (encode_warn)
	warnRd(Rd, Rdfile, gettextf("Some input could not be re-encoded to %s",
	       outputEncoding))
    if (concordance) {
    	conc$srcFile <- Rdfile
        concdata <- followConcordance(conc$finish(), attr(Rd, "concordance"))
        attr(out, "concordance") <- concdata
    }

    invisible(structure(out, latexEncoding = latexEncoding,
                        hasFigures = hasFigures))
}

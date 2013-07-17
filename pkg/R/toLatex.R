################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

## functions for coercion to LaTeX "objects"

toLatex.character <- function(object, ...) {
    result <- object
    result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
    result <- gsub("$", "\\$", result, fixed = TRUE)
    result <- gsub(">=", "$\\geq$", result, fixed = TRUE)
    result <- gsub("<=", "$\\leq$", result, fixed = TRUE)
    result <- gsub(">", "$>$", result, fixed = TRUE)
    result <- gsub("<", "$<$", result, fixed = TRUE)
    result <- gsub("|", "$|$", result, fixed = TRUE)
    result <- gsub("{", "\\{", result, fixed = TRUE)
    result <- gsub("}", "\\}", result, fixed = TRUE)
    result <- gsub("%", "\\%", result, fixed = TRUE)
    result <- gsub("&", "\\&", result, fixed = TRUE)
    result <- gsub("_", "\\_", result, fixed = TRUE)
    result <- gsub("#", "\\#", result, fixed = TRUE)
    result <- gsub("\\^([[:digit:]]+)", "$^{\\1}$", result)
    result <- gsub("\\^([^[:digit:]])", "\\\\verb|^|\\1", result)
    result <- gsub("~", "\\~{}", result, fixed = TRUE)
    ## grep for ^2 and ^3
    result <- gsub("\u00B2", "$^2$", result, fixed = TRUE)
    result <- gsub("\u00B3", "$^3$", result, fixed = TRUE)
    result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$",
                   result, fixed = TRUE)
    return(result)
}


## based on toLatex.sessionInfo from package utils
toLatex.sessionInfo <- function(object, pkgs = NULL, locale = FALSE,
                                base.pkgs = FALSE, other.pkgs = TRUE,
                                namespace.pks = FALSE, citations = TRUE,
                                citecommand = "\\citep", file = "Rpackages.bib",
                                append = FALSE, ...) {
    if (!is.null(pkgs)) {
        object <- sessionInfo(package = pkgs)
        if (!other.pkgs)
            warning(sQuote("other.pkgs"), " should be TRUE if ",
                    sQuote("pkgs"), " is specified.")
    }

    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)

    if (citations)
        key <- write.bib("base", file = "Rpackages.bib", append = append, verbose = FALSE)$key

    z <- c("\\begin{itemize}\\raggedright",
           paste0("  \\item ", object$R.version$version.string,
                  if (!is.null(key)) paste0(citecommand, "{", key, "}")))

    if (locale) {
        z <- c(z, paste0("  \\item Locale: \\verb|",
                         gsub(";", "|, \\\\verb|", object$locale), "|"))
    }

    if (base.pkgs) {
        z <- c(z, strwrap(paste("\\item Base packages: ",
                                paste(sort(object$basePkgs), collapse = ", ")),
                          indent = 2, exdent = 4))
    }
    if (other.pkgs && length(opkgver)) {
        if (is.null(pkgs))
            opkgver <- opkgver[sort(names(opkgver))]
        if (citations) {
            key <- write.bib(names(opkgver), file = "Rpackages.bib",
                             append = TRUE, verbose = FALSE)$key
        }
        z <- c(z, "  \\item Used packages: ", "  \\begin{itemize}",
               formatPkgs(names(opkgver), opkgver, key), "  \\end{itemize}")
    }
    if (namespace.pks && length(nspkgver)) {
        nspkgver <- nspkgver[sort(names(nspkgver))]
        if (citations)
            key <- write.bib(names(nspkgver), file = "Rpackages.bib",
                             append = TRUE, verbose = FALSE)$key
        z <- c(z, "  \\item Loaded via a namespace (and not attached): ",
               "  \\begin{itemize}",
               formatPkgs(names(nspkgver), nspkgver, key), "  \\end{itemize}")
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "Latex"
    z
}

formatPkgs <- function(name, vers, key, citecommand = "\\citep") {
    key <- sapply(name, function(x)
                  paste(key[grep(paste0("^pkg:", x, "[[:digit:]]*$"), key)],
                        collapse = ","))
    cites <- paste0(citecommand, "{", key, "}")
    cites[is.null(key)] <- ""
    paste0("\\item ", name, " (vers. ", vers, ") ", cites)
}

## based on package bibtex
write.bib <- function(entry = "base", file = NULL,
                      append = FALSE, verbose = TRUE) {

    ## define bibs
    bibs <- if (inherits(entry, "bibentry")) {
        entry
    } else {
        if (is.character(entry)) {
            if (length(entry) == 0) {
                if (verbose)
                    message("Empty package list: nothing to be done.")
                return(invisible())
            }
            ## save names of packages
            pkgs <- entry
            bibs <- sapply(pkgs, function(x) try(citation(x)), simplify = FALSE)
            n.installed <- length(bibs)
            ok <- sapply(bibs, inherits, "bibentry")
            pkgs <- pkgs[ok]
            bibs <- bibs[ok]
            n.converted <- sum(ok)
            ## generate unique keys
            pkgs <- lapply(seq_along(pkgs), function(i)
                           if (length(bibs[[i]]) > 1) {
                               paste0(pkgs[i], 1:length(bibs[[i]]))
                           } else {
                               pkgs[i]
                           })
            pkgs <- do.call("c", pkgs)
            bibs <- do.call("c", bibs)
            ## add keys to bibentries
            bibs <- mapply(function(b, k) {
                b$key <- paste0("pkg:", k)
                b
            }, bibs, pkgs, SIMPLIFY = FALSE)
            bibs <- do.call("c", bibs)
            if (verbose)
                message("Converted ", n.converted, " of ", n.installed,
                        " package citations to BibTeX")
            bibs
        } else {
            stop("Invalid argument 'entry': ",
                 "expected a bibentry object or a character vector ",
                 "of package names.")
        }
    }

    if (length(bibs) == 0) {
        if (verbose)
            message("Empty bibentry list: nothing to be done.")
        return(invisible())
    }
    if (!is.null(file)) {
        if (is.character(file)) {
            if (!grepl("\\.bib$", file))
                file <- paste(file, ".bib", sep = "")
        }
        fh <- file(file, open = ifelse(append, "a+", "w+"))
        on.exit(if (isOpen(fh)) close(fh))
        if (verbose)
            message("Writing ", length(bibs), " Bibtex entries ... ",
                    appendLF = FALSE)
        writeLines(toBibtex(bibs), fh)
        if (verbose)
            message("OK\nResults written to file '", file, "'")
        return(invisible(bibs))
    } else {
        return(bibs)
    }
}

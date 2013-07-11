################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

### for package testing:
# pkgEnv <- getNamespace("papeR")
# attach(pkgEnv)


################################################################################
# LaTeX Tables with Descriptves for Continuous Variables
latex.table.cont <- function(data, variables = names(data),
                             labels = NULL, group = NULL,
                             colnames = NULL, digits = 2,
                             table = c("tabular", "longtable"), align = NULL,
                             caption = NULL, label = NULL, floating = FALSE,
                             center = TRUE, sep = !is.null(group),
                             sanitize = TRUE,
                             count = TRUE, mean_sd = TRUE, quantiles = TRUE,
                             incl_outliers = TRUE, drop = TRUE,
                             show.NAs = any(is.na(data[, variables]))) {

    table <- match.arg(table)
    if (is.null(labels)) {
        labels <- variables
    } else {
        if (is.logical(labels) && labels) {
            labels <- labels(data, which = variables)
        } else {
            if (length(variables) != length(labels))
                stop(sQuote("variables"), " and ", sQuote("labels"),
                     " must have the same length")
        }
    }

    if (!is.null(group)) {
        if(!is.factor(data[, group]))
            stop(sQuote("group"), " must be a factor variable")
        if (group %in% variables) {
            idx <- variables != group
            variables <- variables[idx]
            labels <- labels[idx]
        }
        group_var <- data[, group]
    }
    ## get numerical variables
    num <- mySapply(data[, variables], is.numeric)
    ## drop missings
    if (drop) {
        compl.missing <- mySapply(data[, variables], function(x) all(is.na(x)))
        num <- num & !compl.missing
    }

    ## if not any is TRUE (i.e. all are FALSE):
    if (!any(count, mean_sd, quantiles))
        stop("Nothing to compute. All quantities are set to FALSE.")
    ## if all variables are factors:
    if (all(!num))
        stop("None of the variables is numeric or all variables are missing. Nothing to compute.")
    ## if factors are dropped:
    if (any(!num))
        warning("Factors are dropped from the summary")

    ## subset variables to non-factors only
    variables <- variables[num]
    labels <- labels[num]

    ## setup results object
    sums <- data.frame(variable = labels, group = NA, blank = "",
                       N=NA, Missing = NA, blank_1 = "",
                       Mean=NA, SD=NA, blank_2 = "",
                       Min=NA, Q1=NA, Median=NA, Q3=NA, Max=NA, var = variables,
                       stringsAsFactors = FALSE)

    if (!is.null(group)) {
        sums <- sums[rep(1:nrow(sums), each = nlevels(group_var)), ]
        sums$group <- levels(group_var)
    } else {
        ## drop group variable
        sums$group <- NULL
        sums$blank <- NULL
    }

    myData <- data
    ## compute statistics
    for (i in 1:nrow(sums)){
        if (!is.null(group)) {
            myData <- data[group_var == sums$group[i], ]
        }
        sums$N[i] <- sum(!is.na(myData[, sums$var[i]]))
        sums$Missing[i] <- sum(is.na(myData[, sums$var[i]]))
        sums$Mean[i] <- round(mean(myData[, sums$var[i]], na.rm=TRUE),
                              digits = digits)
        sums$SD[i] <- round(sd(myData[, sums$var[i]],
                               na.rm=TRUE), digits = digits)
        if (incl_outliers) {
            Q <- round(fivenum(myData[, sums$var[i]]), digits = digits)
        } else {
            Q <- round(c(boxplot(myData[, sums$var[i]], plot = FALSE)$stats),
                       digits = digits)
        }
        sums$Min[i] <- Q[1]
        sums$Q1[i]  <- Q[2]
        sums$Median[i] <- Q[3]
        sums$Q3[i] <- Q[4]
        sums$Max[i] <- Q[5]
    }

    ## remove superfluous variables
    sums$var <- NULL
    if (!show.NAs) {
        sums$Missing <- NULL
    }
    if (!is.null(group)) {
        names(sums)[names(sums) == "group"] <- labels(data, group)
    }

    add_options(sums, table = table, align = align, caption = caption,
                label = label, floating = floating, center = center, sep = sep,
                sanitize = sanitize,
                count = count, mean_sd = mean_sd, quantiles = quantiles,
                colnames = colnames, class = "table.cont")
}

################################################################################
# LaTeX Tables with Descriptves for Factor Variables
latex.table.fac <- function(data, variables = names(data),
                            labels = NULL, group = NULL,
                            colnames = NULL, digits = 2,
                            table = c("tabular", "longtable"),
                            align = NULL,
                            caption = NULL, label = NULL, floating = FALSE,
                            center = TRUE,
                            sep = TRUE, sanitize = TRUE,
                            drop = TRUE,
                            show.NAs = any(is.na(data[, variables])),
                            na.lab = "<Missing>") {

    if (!is.null(group)) {
        if(!is.factor(data[, group]))
            stop(sQuote("group"), " must be a factor variable")
        if (group %in% variables) {
            idx <- variables != group
            variables <- variables[idx]
            labels <- labels[idx]
        }
        group_var <- data[, group]

        cl <- match.call()
        cl[["group"]] <- NULL
        print_single_tabs <- function(level, data, grp_var) {
            dat <- data[grp_var == level, ]
            cl[["data"]] <- dat
            cl[["caption"]] <- paste(caption, " (", group , ": ", level, ")",
                                     sep ="")
            if (!is.null(label))
                cl[["label"]] <- paste(label, level, sep = "_")
            eval(cl)
        }
        res <- lapply(levels(group_var), print_single_tabs,
                      data = data, grp_var = group_var)
        class(res) <- "table.list"
        return(res)
    }

    table <- match.arg(table)
    if (is.null(labels)) {
        labels <- variables
    } else {
        if (is.logical(labels) && labels) {
            labels <- labels(data, which = variables)
        } else {
            if (length(variables) != length(labels))
                stop(sQuote("variables"), " and ", sQuote("labels"),
                     " must have the same length")
        }
    }

    ## get factors
    fac <- mySapply(data[, variables], is.factor)
    ## drop missings
    if (drop) {
        compl.missing <- mySapply(data[, variables], function(x) all(is.na(x)))
        fac <- fac & !compl.missing
    }

    ## if all variables are not factors:
    if (all(!fac))
        stop("None of the variables is a factor or all variables are missing. Nothing to compute.")
    ## if non-factors are dropped:
    if (any(!fac))
        warning("Non-factors are dropped from the summary")

    ## subset variables to non-factors only
    variables <- variables[fac]
    labels <- labels[fac]

    if (show.NAs) {
        ## convert NAs to factor levels
        NAtoLvl <- function(x){
            if (any(is.na(x))) {
                lvls <- levels(x)
                x <- as.character(x)
                x[is.na(x)] <- na.lab
                return(factor(x, levels = c(lvls, na.lab)))
            }
            return(x)
        }
        if (length(variables) > 1) {
            data[, variables] <- as.data.frame(lapply(data[, variables], NAtoLvl))
        } else {
            data[, variables] <- NAtoLvl(data[, variables])
        }
    }

    ## repeate variables to match no. of levels:
    n.levels <- mySapply(data[, variables], function(x) length(levels(x)))

    var2 <- unlist(lapply(1:length(variables),
                          function(i) rep(variables[i], each = n.levels[i])))
    var_labels <- unlist(lapply(1:length(variables),
                                function(i) rep(labels[i], each = n.levels[i])))

    ## get all levels
    lvls <- unlist(lapply(variables, function(x) levels(data[, x])))
    colnames(lvls) <- NULL

    ## setup results object
    stats <- data.frame(variable = var_labels, Level = lvls, blank = "",
                        N = NA, blank_2 = "",
                        Fraction = NA, CumSum = NA, blank_3 = "",
                        Fraction_2 = NA, CumSum_2 = NA,
                        stringsAsFactors = FALSE)
    if (!show.NAs) {
            stats$Fraction_2 <- NULL
            stats$CumSum_2 <- NULL
            stats$blank_2 <- NULL
            stats$blank_3 <- NULL
    }
    rownames(stats) <- NULL

    ## compute statistics
    for (i in 1:length(var2)) {
        notna <- sum(!is.na(data[, var2[i]]))
        stats$N[i] <- sum(data[, var2[i]] == lvls[i], na.rm = TRUE)
        stats$Fraction[i] <- round(stats$N[i]/notna, digits = digits)
        stats$CumSum[i] <- sum(stats$Fraction[1:i][var2[1:i] == var2[i]])
        if (show.NAs && lvls[i] != na.lab) {
            notna_2 <- sum(data[, var2[i]] != na.lab)
            stats$Fraction_2[i] <- round(stats$N[i]/notna_2, digits = digits)
            stats$CumSum_2[i] <- sum(stats$Fraction_2[1:i][var2[1:i] == var2[i]])
        }
    }
    add_options(stats, table = table, align = align, caption = caption,
                label = label, floating = floating, center = center,
                sep = sep, sanitize = sanitize, colnames = colnames,
                class = "table.fac")
}

################################################################################
## Helper for latex.table.cont
print.table.cont <- function(x,
                             colnames = get_options(x, "colnames"),
                             table = get_options(x, "table"),
                             align = get_options(x, "align"),
                             caption = get_options(x, "caption"),
                             label = get_options(x, "label"),
                             floating = get_options(x, "floating"),
                             center = get_options(x, "center"),
                             sep = get_options(x, "sep"),
                             sanitize = get_options(x, "sanitize"),
                             count = get_options(x, "count"),
                             mean_sd = get_options(x, "mean_sd"),
                             quantiles = get_options(x, "quantiles"),
                             ...) {

    tab <- x
    ## drop duplicted variable names
    tmp <- tab$variable
    tmp[duplicated(tmp)] <- ""
    tab$variable <- tmp

    ## if not all are TRUE subset results object
    if (!all(count, mean_sd, quantiles)) {

        ## if not any is TRUE (i.e. all are FALSE):
        if (!any(count, mean_sd, quantiles)) {
            stop("Nothing to compute. All quantities are set to FALSE.")
        }
        if (count == FALSE) {
            tab$N <- NULL
            tab$Missing <- NULL
        }
        if (mean_sd == FALSE) {
            tab$Mean <- NULL
            tab$SD <- NULL
        }
        if (quantiles == FALSE) {
            tab$Min <- NULL
            tab$Q1 <- NULL
            tab$Median <- NULL
            tab$Q3 <- NULL
            tab$Max <- NULL
        }
        if (count == FALSE || (mean_sd == FALSE && quantiles == FALSE)) {
            tab$blank_1 <- NULL
        }
        if (mean_sd == FALSE || quantiles == FALSE) {
            tab$blank_2 <- NULL
        }
    }

    if (any(names(tab) == "blank")) {
        start <- which(names(tab) == "blank") + 1
    } else {
        start <- 2
    }
    if (any(grepl("blank_", names(tab)))) {
        idx <- grep("blank_", names(tab))
        if (length(idx) == 1) {
            rules <- paste("  \\cmidrule{", start, "-", idx - 1, "}  ",
                           "\\cmidrule{", idx + 1, "-", length(names(tab)), "}\n",
                           sep = "")
        } else {
            rules <- paste("  \\cmidrule{", start, "-", idx[1] - 1, "}  ",
                           "\\cmidrule{", idx[1] + 1, "-", idx[2] - 1, "} ",
                           "\\cmidrule{", idx[2] + 1, "-", length(names(tab)), "}\n",
                           sep = "")
        }
    } else {
        rules <- paste("  \\cmidrule{", start, "-", length(names(tab)), "}\n",
                       sep = "")
    }
    if (is.null(align))
        align <- paste("l",
                       paste(rep("r", length(names(tab)) - 1), collapse = ""),
                       sep = "")

    ## Define column names
    if (!is.null(colnames)) {
        colNames <- names(tab)
        ## blank doesn't need to be specified in colnames
        if (sum(nms <- !grepl("blank", colNames)) != length(colnames))
            stop(sQuote("colnames"), " has wrong length")
        colNames[nms] <- colnames
    } else {
        colNames <- names(tab)
        colNames[1] <- " "
    }
    colNames[grep("blank", colNames)] <- " "

    ## start printing
    print_table(tab = tab, table = table, floating = floating,
                caption = caption, label = label, center = center, sep = sep,
                sanitize = sanitize, align = align, colNames = colNames,
                rules = rules, header = NULL)
}


################################################################################
## Helper for latex.table.fac
print.table.fac <- function(x,
                            colnames = get_options(x, "colnames"),
                            table = get_options(x, "table"),
                            align = get_options(x, "align"),
                            caption = get_options(x, "caption"),
                            label = get_options(x, "label"),
                            floating = get_options(x, "floating"),
                            center = get_options(x, "center"),
                            sep = get_options(x, "sep"),
                            sanitize = get_options(x, "sanitize"),
                            ...) {

    tab <- x
    ## drop duplicted variable names
    tmp <- tab$variable
    tmp[duplicated(tmp)] <- ""
    tab$variable <- tmp

    idx <- grep("blank", names(tab))
    if (length(idx) == 1) {
        rules <- paste("  \\cmidrule{2-2} \\cmidrule{4-", ncol(tab), "}\n", sep = "")
    } else {
        rules <- paste("  \\cmidrule{2-2}",
                       "\\cmidrule{", idx[1] + 1, "-", idx[2] - 1, "} ",
                       "\\cmidrule{", idx[2] + 1, "-", idx[3] - 1, "} ",
                       "\\cmidrule{", idx[3] + 1, "-", length(names(tab)), "}\n",
                       sep = "")
    }
    if (is.null(align))
        align <- paste("ll",
                       paste(rep("r", length(names(tab)) - 2), collapse = ""),
                       sep = "")

    ## Define column names
    if (!is.null(colnames)) {
        colNames <- names(tab)
        ## blank doesn't need to be specified in colnames
        if (sum(nms <- !grepl("blank", colNames)) != length(colnames))
            stop(sQuote("colnames"), " has wrong length")
        colNames[nms] <- colnames
    } else {
        colNames <- names(tab)
        colNames[colNames == "Fraction"] <- "\\%"
        colNames[colNames == "CumSum"] <- "$\\sum$ \\%"
        colNames[colNames == "Fraction_2"] <- "\\%"
        colNames[colNames == "CumSum_2"] <- "$\\sum$ \\%"
        colNames[1] <- " "
        if (ncol(tab) == 10) {
            header <- paste(paste(rep("&", 5), collapse =" "),
                            "\\multicolumn{2}{c}{(incl. Missings)}  & & \\multicolumn{2}{c}{(w/o Missings)} \\\\")
        }
    }
    colNames[grep("blank", colNames)] <- " "

    if (!exists("header"))
        header <- NULL
    ## start printing
    print_table(tab = tab, table = table, floating = floating,
                caption = caption, label = label, center = center,
                align = align, colNames = colNames, rules = rules,
                sep = sep, sanitize = sanitize, header = header)
}


print_table <- function(tab, table, floating, caption, label,
                        center, align, colNames, rules, sep, sanitize, header) {
    cat("%% Output requires \\usepackage{booktabs}.\n")
    if (table == "longtable")
        cat("%% Output requires \\usepackage{longtable}.\n")
    if (floating) {
        if (table == "longtable") {
            warning(sQuote("floating = TRUE"), " cannot be used with ",
                    dQuote("longtable"))
        } else {
            cat("\\begin{table}")
            if (!is.null(caption))
                cat("  \\caption{", caption, "}\n\n")
            if (!is.null(label))
                cat("  \\label{", label, "}\n\n")
            if (center)
                cat("\\begin{center}\n")
        }
    } else {
        if (!is.null(caption)) {
            if (table == "longtable") {
                if (center)
                    cat("\\begin{center}\n")
                cat("\\begin{", table,"}{", align, "} \n", sep ="")
                cat("  \\caption{", caption, "}\n")
                if (!is.null(label))
                    cat("  \\label{", label, "}\n")
                cat("\\\\[-1em]\n")
            } else {
                cat("%% Output requires \\usepackage{capt-of}.\n")
                ## use end minipage to group caption and table
                cat("\\begin{minipage}{\\linewidth}\n")
                if (center)
                    cat("\\begin{center}\n")
                cat("  \\captionof{table}{", caption, "}\n")
                if (!is.null(label))
                    cat("  \\label{", label, "}\n")
                #if (center)
                #    cat("  \\vspace*{-1em}\n")
                cat("\\begin{", table,"}{", align, "} \n", sep ="")
            }
        } else {
            if (center)
                cat("\\begin{center}\n")
            cat("\\begin{", table,"}{", align, "} \n", sep ="")
        }
    }
    cat("  \\toprule \n")

    if (!is.null(header))
        cat(header, " \n")

    cat(paste(colNames, collapse = " & "), "\\\\ \n")
    cat(rules)
    if (table == "longtable")
        cat("  \\endhead  \n")

    ## Convert to character strings
    tab <- apply(tab, 2, function(x)
                 ifelse(sapply(x, is.numeric), sprintf("%4.2f", x), x))
    ## if tab accidentially drops to a vector
    if (is.null(dim(tab)))
        tab <- matrix(tab, nrow = 1)

    # tab[,1] <- gsub("(_)", "\\\\_", tab[,1])
    if (is.function(is.function(sanitize))) {
        toLatex <- sanitize
        sanitize <- TRUE
    }
    if (is.logical(sanitize) && sanitize == TRUE)
        tab <- apply(tab, 2, toLatex)
    ## if tab accidentially drops to a vector
    if (is.null(dim(tab)))
        tab <- matrix(tab, nrow = 1)

    ## if separators should be added after each factor variable:
    if (sep) {
        tab[tab[,1] != "", 1][-1] <- paste(rules, tab[tab[,1] != "", 1][-1])
    }

    ## Replace NA with " "
    tab[is.na(tab)] <- ""
    out <- apply(tab, 1, function(x)
                 cat(paste(x, collapse = " & "), " \\\\ \n"))
    cat("  \\bottomrule \n")

    cat("\\end{", table, "} \n\n", sep ="")
    if (center)
        cat("\\end{center}\n")
    if (floating && table != "longtable")
        cat("\\end{table}\n\n")
    ## if captionof is used end minipage
    if (!floating && table != "longtable" && !is.null(caption))
        cat("\\end{minipage}\n")
}

print.table.list <- function(x, ...) {
    lapply(x, print)
}

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
                                ...) {
    if (!is.null(pkgs)) {
        object <- sessionInfo(package = pkgs)
        if (!other.pkgs)
            warning(sQuote("other.pkgs"), " should be TRUE if ",
                    sQuote("pkgs"), " is specified.")
    }

    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)

    if (citations)
        key <- write.bib("base", file = "Rpackages.bib", append = FALSE, verbose = FALSE)$key

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

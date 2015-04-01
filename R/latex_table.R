################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# LaTeX Tables with Descriptves for Continuous Variables
latex.table.cont <- function(data, variables = names(data),
                             labels = NULL, group = NULL,
                             test = TRUE,
                             colnames = NULL, digits = 2,
                             digits.pval = 3, smallest.pval = 0.001,
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
        ## remove observations with missing group:
        if (any(is.na(data[, group]))) {
            warning("Removed observations with missing group")
            data <- data[!is.na(data[, group]), ]
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

    if (!is.null(group)) {
        if (!is.character(test) && test)
            test <- "t.test"

        if (all(is.character(test))) {
            if (length(test) == 1)
                test <- rep(test, length(variables))

            pval <- rep(NA, length(variables))
            for (i in 1:length(variables)) {
                fm <- as.formula(paste(variables[i], " ~ ", group))
                pval[i] <- do.call(test[i], list(formula = fm, data = data))$p.value
            }
            ## make sure rounding is to digits.pval digits
            pval <- format.pval(round(pval, digits = digits.pval),
                                eps = smallest.pval)
            ## make sure not to drop trailing zeros
            pval2 <- suppressWarnings(as.numeric(pval))
            pval[is.na(pval2)] <- sprintf(paste0("%0.", digits.pval, "f"),
                                          pval2[is.na(pval2)])
            sums$blank_p <- ""
            sums$p.value[!duplicated(sums$var)] <- pval
        }
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
                            test = TRUE, colnames = NULL, digits = 3,
                            digits.pval = 3, smallest.pval = 0.001,
                            table = c("tabular", "longtable"),
                            percent = TRUE, cumulative = FALSE,
                            align = NULL,
                            caption = NULL, label = NULL, floating = FALSE,
                            center = TRUE,
                            sep = TRUE, sanitize = TRUE,
                            drop = TRUE,
                            show.NAs = any(is.na(data[, variables])),
                            na.lab = "<Missing>") {

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
        if (length(variables) > 1) {
            data[, variables] <- as.data.frame(lapply(data[, variables], NAtoLvl, na.lab = na.lab))
        } else {
            data[, variables] <- NAtoLvl(data[, variables], na.lab)
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
        ## remove observations with missing group:
        if (any(is.na(data[, group]))) {
            warning("Removed observations with missing group")
            data <- data[!is.na(data[, group]), ]
        }
        group_var <- data[, group]

        cl <- match.call()
        cl[["group"]] <- NULL
        ## modify call to obtain results for grouped data
        print_single_tabs <- function(level, data, grp_var) {
            dat <- data[grp_var == level, ]
            ## make sure no fatcor level is dropped
            dat <- keep_levels(dat, data)
            cl[["data"]] <- dat
            ## test is not needed in single tables
            cl[["test"]] <- FALSE
            if (!is.null(label))
                cl[["label"]] <- paste(label, level, sep = "_")
            ## re-evaluate modified call
            eval(cl)
        }
        res <- lapply(levels(group_var), print_single_tabs,
                      data = data, grp_var = group_var)

        # if (length(res) != 2)
        #     stop("Combining more than 2 groups not yet implemented")
        # tab <- cbind(res[[1]], res[[2]][, -c(1:2)])

        res[-1] <- lapply(res[-1], function(x) x[, -c(1:2)])
        tab <- do.call("cbind", res)

        if (!is.character(test) && test)
            test <- "fisher.test"

        if (all(is.character(test))) {
            if (length(test) == 1)
                test <- rep(test, length(variables))
            testdat <- as.matrix(tab[, grep("N", colnames(tab))])
            pval <- rep(NA, length(variables))
            for (i in 1:length(variables)) {
                test_tab <- testdat[tab$variable == unique(tab$variable)[i] & tab$Level != na.lab, ]
                pval[i] <- eval(call(test[i], test_tab))$p.value
            }
            ## make sure rounding is to digits.pval digits
            pval <- format.pval(round(pval, digits = digits.pval),
                                eps = smallest.pval)
            ## make sure not to drop trailing zeros
            pval2 <- suppressWarnings(as.numeric(pval))
            pval[is.na(pval2)] <- sprintf(paste0("%0.", digits.pval, "f"),
                                          pval2[is.na(pval2)])
            tab$blank_p <- ""
            tab$p.value[!duplicated(tab$variable)] <- pval
        }

        attr(tab, "latex.table.options") <- attr(res[[1]], "latex.table.options")
        attr(tab, "group_labels") <- paste(group, levels(group_var), sep = ": ")
        class(tab) <- c("table.fac", "data.frame")
        return(tab)
    }

    ## test not sensible
    if (test || is.character(test))
        warning(sQuote("test"), " is ignored if no ", sQuote("group"), " is given")

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
                        N = NA, Fraction = NA, CumSum = NA,
                        stringsAsFactors = FALSE)
    if (!cumulative) {
        stats$CumSum <- NULL
    }
    rownames(stats) <- NULL

    ## compute statistics
    for (i in 1:length(var2)) {
        notna <- sum(!is.na(data[, var2[i]]))
        stats$N[i] <- sum(data[, var2[i]] == lvls[i], na.rm = TRUE)
        stats$Fraction[i] <- round(stats$N[i]/notna, digits = digits)
        if (cumulative)
            stats$CumSum[i] <- sum(stats$Fraction[1:i][var2[1:i] == var2[i]])
    }
    if (percent) {
        stats$Fraction <- sprintf(paste0("%3.", digits - 2,"f"),
                                  stats$Fraction * 100)
        if (cumulative)
            stats$CumSum <- sprintf(paste0("%3.", digits - 2,"f"),
                                    stats$CumSum * 100)
    } else {
        stats$Fraction <- sprintf(paste0("%1.", digits,"f"), stats$Fraction)
        if (cumulative)
            stats$CumSum <- sprintf(paste0("%1.", digits,"f"), stats$CumSum)
    }
    add_options(stats, table = table, align = align, caption = caption,
                label = label, floating = floating, center = center,
                sep = sep, sanitize = sanitize, colnames = colnames,
                percent = percent, class = "table.fac")
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

    ## define rules
    idx <- c(grep("blank", names(tab)), length(names(tab)) + 1)
    rules <- "  \\cmidrule{2-2} "
    for (i in 1:(length(idx) - 1)) {
        rules <- paste0(rules, "\\cmidrule{", idx[i] + 1, "-", idx[i+1] - 1, "} ")
    }
    rules <- paste0(rules, "\n")

    if (is.null(align))
        align <- paste("ll",
                       paste(rep("r", length(names(tab)) - 2), collapse = ""),
                       sep = "")

    ## Define column names
    if (!is.null(colnames)) {
        colNames <- names(tab)
        ## blank doesn't need to be specified in colnames
        if (sum(nms <- !grepl("blank", colNames)) != length(colnames))
            stop(sQuote("colnames"), " has wrong length (should be", sum(nms), ")")
        colNames[nms] <- colnames
    } else {
        colNames <- names(tab)
        if (get_options(x, "percent")) {
            colNames[grepl("Fraction", colNames)] <- "\\%"
            colNames[grepl("CumSum", colNames)] <- "$\\sum$ \\%"
        } else {
            colNames[grepl("CumSum", colNames)] <- "$\\sum$"
        }
        colNames[1] <- " "

        header <- ""
        ## if more than one blank add group label
        if (!is.null(attr(tab, "group_labels"))) {
            lab <- attr(tab, "group_labels")
            ## if p.values exist last multicolumn
            ## should not include this column
            if (colNames[length(colNames)] == "p.value")
                idx <- idx[-length(idx)]
            header <- paste(rep("&", idx[1]), collapse = " ")
            for (i in 1:(length(idx) - 1)) {
                header <- paste0(header, "\\multicolumn{", idx[i+1] - idx[i] - 1, "}{c}{", lab[i],"}")
                if (i != length(idx) - 1)
                    header <- paste0(header, " & & ")
            }
            if (colNames[length(colNames)] == "p.value")
                header <- paste0(header, " &  & ")
            header <- paste0(header, "\\\\\n")
        }
    }
    colNames[grep("blank", colNames)] <- " "

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

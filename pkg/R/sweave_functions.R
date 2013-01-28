################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# Extract labels from data sets
labels.data.frame <- function(object, which = NULL, ...){
    if (is.null(which))
        return(attr(object, "variable.labels"))

    if (is.null(attr(object, "variable.labels"))) {
        warning("No labels defined")
    } else {
        if (is.numeric(which) && any(which > length(attr(object, "variable.labels"))) ||
            is.character(which) && !all(which %in% names(attr(object, "variable.labels"))))
            stop("One cannot extract labels for non-existing variables.")
    }
    return(attr(object, "variable.labels")[which])
}

################################################################################
# Sets labels as attribute "variable.labels"
"labels<-" <- function(data, which = NULL, value){

    if (is.null(which)) {
        if (!is.null(value) && ncol(data) != length(value))
            stop("You must supply a label for each column of the data set\n",
                 "or use argument ", sQuote("which"))
        attr(data, "variable.labels") <- value
        ## set label names to variable names
        if (!is.null(value) && is.null(names(attr(data, "variable.labels"))))
            names(attr(data, "variable.labels")) <- colnames(data)
        return(data)
    }

    ## if partial replacement is used and no labels are given so far
    ## a "dummy" vector is created
    if (is.null(attr(data, "variable.labels"))) {
        attr(data, "variable.labels") <- colnames(data)
        names(attr(data, "variable.labels")) <- colnames(data)
    }

    if (is.null(value))
        stop(sQuote("NULL"), " cannot be assigned in combination with ", sQuote("which"))
    if (is.numeric(which) && any(which > ncol(data)) ||
        is.character(which) && !all(which %in% colnames(data)))
        stop("One  cannot supply labels for none-existing variables")
    if (length(which) != length(value))
        stop("You must supply a label for each selected column of the data set.")

    attr(data, "variable.labels")[which] <- value
    return(data)
}

"labels[<-" <- function(data, i, value){
    labels(data, which = i) <- value
}


################################################################################
# Extract Fixed Effects from Mixed Models
# (set generic function)
summary.fixef <- function(object, ...)
    UseMethod("summary.fixef")

################################################################################
# Extract Fixed Effects from lme Objects (package nlme)
# Based on modified code from nlme:::print.summary.lme
summary.fixef.lme <- function(object, digits = NULL, scientific = FALSE,
                              smallest.pval = 0.001, ...){
    x <- summary(object)
    xtTab <- as.data.frame(x$tTable)
    wchPval <- match("p-value", names(xtTab))
    for (i in names(xtTab)[-wchPval]) {
        xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                             scientific = scientific, ...)
    }
    if (!is.na(wchPval)) {
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        xtTab[, wchPval] <- format.pval(round(xtTab[, wchPval], digits = r.digits),
                                        digits = digits, scientific = scientific,
                                        eps = smallest.pval, ...)
    } else {
        warning("No p-value detected.")
    }
    row.names(xtTab) <- dimnames(x$tTable)[[1]]
    xtTab
}

################################################################################
# Extract Fixed Effects from mer Objects (package lme4)
# Based on modified code from nlme:::print.summary.lme
summary.fixef.mer <- function(object, digits = NULL, scientific = FALSE,
                              smallest.pval = 0.001, ...){
    x <- summary(object)
    xtTab <- as.data.frame(x@coefs)
    wchPval <- match("Pr(>|z|)", names(xtTab))
    for (i in names(xtTab)[-wchPval]) {
        xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                             scientific = scientific, ...)
    }
    if (!is.na(wchPval)) {
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        xtTab[, wchPval] <- format.pval(round(xtTab[, wchPval], digits = r.digits),
                                        digits = digits, scientific = scientific,
                                        eps = smallest.pval, ...)
    } else {
        warning("No p-value detected.")
    }
    row.names(xtTab) <- dimnames(x@coefs)[[1]]
    xtTab
}


################################################################################
# LaTeX Tables with Descriptves for Continuous Variables
latex.table.cont <- function(data, variables = names(data),
                             colnames = NULL, digits = 2,
                             table = c("tabular", "longtable"),
                             align = NULL,
                             count = TRUE, mean_sd = TRUE, quantiles = TRUE,
                             incl_outliers = TRUE, drop = TRUE,
                             show.NAs = any(is.na(data[, variables]))) {

    table <- match.arg(table)

    ## get factors
    fac <- mySapply(data[, variables], is.factor)
    ## drop missings
    if (drop) {
        compl.missing <- mySapply(data[, variables], function(x) all(is.na(x)))
        fac <- fac | compl.missing
    }

    ## if not any is TRUE (i.e. all are FALSE):
    if (!any(count, mean_sd, quantiles))
        stop("Nothing to compute. All quantities are set to FALSE.")
    ## if all variables are factors:
    if (all(fac))
        stop("All variables are factors. Nothing to compute.")
    ## if factors are dropped:
    if (any(fac))
        warning("Factors are dropped from the summary")

    ## subset variables to non-factors only
    variables <- variables[!fac]

    ## setup results object
    sums <- data.frame(variable = variables, N=NA, Missing = NA, blank_1 = "",
                       Mean=NA, SD=NA, blank_2 = "",
                       Min=NA, Q1=NA, Median=NA, Q3=NA, Max=NA)

    ## compute statistics
    for (i in 1:nrow(sums)){
        sums$N[i] <- sum(!is.na(data[, variables[i]]))
        sums$Missing[i] <- sum(is.na(data[, variables[i]]))
        sums$Mean[i] <- round(mean(data[, variables[i]], na.rm=TRUE),
                              digits = digits)
        sums$SD[i] <- round(sd(data[, variables[i]],
                               na.rm=TRUE), digits = digits)
        if (incl_outliers) {
            Q <- round(fivenum(data[, variables[i]]), digits = digits)
        } else {
            Q <- round(c(boxplot(data[, variables[i]], plot = FALSE)$stats),
                       digits = digits)
        }
        sums$Min[i] <- Q[1]
        sums$Q1[i]  <- Q[2]
        sums$Median[i] <- Q[3]
        sums$Q3[i] <- Q[4]
        sums$Max[i] <- Q[5]
    }

    if (!show.NAs) {
        sums$Missing <- NULL
    }

    ## print results
    cat("%% Output requires \\usepackage{booktabs}.\n")
    printtab(sums, table = table, align = align,
             count = count, mean_sd = mean_sd, quantiles = quantiles,
             colnames = colnames)
    invisible(sums)
}

################################################################################
## Helper for latex.table.cont
printtab <- function(tab, colnames = NULL,
                     table = c("tabular", "longtable"),
                     align = NULL,
                     count = TRUE, mean_sd = TRUE, quantiles = TRUE) {

    table <- match.arg(table)

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

    if (any(grepl("blank", names(tab)))) {
        idx <- grep("blank", names(tab))
        if (length(idx) == 1) {
            rules <- paste("  \\cmidrule{2-", idx - 1, "}  ",
                           "\\cmidrule{", idx + 1, "-", length(names(tab)), "}\n",
                           sep = "")
        } else {
            rules <- paste("  \\cmidrule{2-", idx[1] - 1, "}  ",
                           "\\cmidrule{", idx[1] + 1, "-", idx[2] - 1, "} ",
                           "\\cmidrule{", idx[2] + 1, "-", length(names(tab)), "}\n",
                           sep = "")
        }
    } else {
        rules <- paste("  \\cmidrule{2-", length(names(tab)), "}\n",
                       sep = "")
    }
    if (is.null(align))
        align <- paste("l",
                       paste(rep("r", length(names(tab)) - 1), collapse = ""),
                       sep = "")

    cat("\\begin{", table,"}{", align, "} \n", sep ="")
    cat("  \\toprule \n")
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
    cat(paste(colNames, collapse = " & "), "\\\\ \n")
    cat(rules)
    if (table == "longtable")
        cat("  \\endhead  \n")
    tab[,1] <- gsub("(_)", "\\\\_", tab[,1])
    tab <- apply(tab, 2, function(x)
                 ifelse(sapply(x, is.numeric), sprintf("%4.2f", x), x))
    ## if tab accidentially drops to a vector
    if (is.null(dim(tab)))
        tab <- matrix(tab, nrow = 1)
    out <- apply(tab, 1, function(x)
                 cat(paste(x, collapse = " & "), " \\\\ \n"))
    cat("  \\bottomrule \n")
    cat("\\end{", table, "} \n\n", sep ="")
}



################################################################################
# LaTeX Tables with Descriptves for Factor Variables
latex.table.fac <- function(data, variables = names(data),
                            colnames = NULL, digits = 2,
                            table = c("tabular", "longtable"),
                            align = NULL, sep = TRUE, drop = TRUE,
                            show.NAs = any(is.na(data[, variables])),
                            na.lab = "<Missing>") {

    table <- match.arg(table)

    if (length(variables) > 1) {
        ## get factors
        fac <- sapply(data[, variables], is.factor)
        ## drop missings
        if (drop) {
            compl.missing <- sapply(data[, variables], function(x) all(is.na(x)))
            fac <- fac & !compl.missing
        }
    } else {
        fac <- is.factor(data[, variables])
        if (drop && all(is.na(data[, variables])))
            stop(sQuote("drop = TRUE"), " but all observations are missing in the specified variable.")
    }

    ## if all variables are not factors:
    if (all(!fac))
        stop("None of the variables is a factor or all variables are missing. Nothing to compute.")
    ## if non-factors are dropped:
    if (any(!fac))
        warning("Non-factors are dropped from the summary")

    ## subset variables to non-factors only
    variables <- variables[fac]

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
    if (length(variables) > 1) {
        n.levels <- sapply(data[, variables], function(x) length(levels(x)))
    } else {
        n.levels <- length(levels(data[, variables]))
    }
    var2 <- unlist(sapply(1:length(variables),
                          function(i) rep(variables[i], each = n.levels[i])))
    ## get all levels
    lvls <- unlist(sapply(variables, function(x) levels(data[, x])))
    colnames(lvls) <- NULL

    ## setup results object
    stats <- data.frame(variable = var2, Level = lvls, blank = "",
                        N = NA, blank_2 = "",
                        Fraction = NA, CumSum = NA, blank_3 = "",
                        Fraction_2 = NA, CumSum_2 = NA)
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

    ## print results
    cat("%% Output requires \\usepackage{booktabs}.\n")
    printtab_fac(stats, table = table, align = align, sep = sep,
                 colnames = colnames)
    invisible(stats)
}

################################################################################
## Helper for latex.table.fac
printtab_fac <- function(tab, colnames = NULL,
                         table = c("tabular", "longtable"),
                         align = NULL, sep = TRUE) {

    table <- match.arg(table)

    ## drop duplicted variable names
    tmp <- tab$variable
    tmp <- as.character(tmp)
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

    cat("\\begin{", table,"}{", align, "} \n", sep ="")
    cat("  \\toprule \n")
    if (!is.null(colnames)) {
        colNames <- names(tab)
        ## blank doesn't need to be specified in colnames
        if (sum(nms <- colNames != "blank") != length(colnames))
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

    ## start printing
    if (exists("header"))
        cat(header, " \n")
    cat(paste(colNames, collapse = " & "), "\\\\ \n")
    cat(rules)
    if (table == "longtable")
        cat("  \\endhead  \n")
    tab[,1] <- gsub("(_)", "\\\\_", tab[,1])
    ## if separators should be added after each factor variable:
    if (sep) {
        tab[tab[,1] != "", 1][-1] <- paste(rules, tab[tab[,1] != "", 1][-1])
    }
    ## Convert to character strings
    tab <- apply(tab, 2, function(x)
                 ifelse(sapply(x, is.numeric), sprintf("%4.2f", x), x))
    ## if tab accidentially drops to a vector
    if (is.null(dim(tab)))
        tab <- matrix(tab, nrow = 1)
    ## Replace NA with " "
    tab[is.na(tab)] <- ""
    out <- apply(tab, 1, function(x)
                 cat(paste(x, collapse = " & "), " \\\\ \n"))
    cat("  \\bottomrule \n")
    cat("\\end{", table, "} \n\n", sep ="")
}


################################################################################
# Marginal Anova function in the fashion of library(car) for mixed models
Anova.lme <- function(mod, type = c("marginal", "sequential"), ...) {
    type <- match.arg(type)
    nlme:::anova.lme(mod, type = type, ...)
}


#################################################################################
# Prettify function for summary tables
prettify <- function(object, ...)
    UseMethod("prettify")

prettify.data.frame <- function(object, labels, sep = ": ", extra.column = FALSE, ...) {
    ## get row names
    nms <- new_nms <- rownames(object)

    ## order labels to avoid matching with substrings
    labels <- labels[rev(order(sapply(names(labels), nchar)))]

    ## make extra column if needed
    if (extra.column) {
        object$varlabel <- " "
        object$"FactorLevel" <- " "
        ## move Factor Levels to the front
        newVars <- (ncol(object) -1):ncol(object)
        object <- cbind(object[, newVars],
                        object[, - newVars])
        names(object)[1] <- " "
        object[,1] <- as.character(object[,1])
        names(object)[2] <- "Factor Level"
        object[,2] <- as.character(object[,2])
    }

    for (i in 1:length(labels)) {
        idx <- grep(names(labels)[i], nms)
        if (!length(idx) == 0){
            ## Is there a factor level?
            if (any(grepl(paste("^",names(labels)[i], "$", sep = ""), nms[idx]))) {
                new_nms[idx] <- gsub(names(labels)[i],
                                     labels[i], nms[idx])
            } else {
                if (extra.column) {
                    spaces <- sapply(1:length(idx), function(i) paste(rep(" ", i), collapse = ""))
                    new_nms[idx] <- gsub(paste("^",names(labels)[i], "(.*)", sep = ""),
                                         paste(labels[i], spaces, sep = ""),
                                         nms[idx])
                    object[idx, 2] <- gsub(paste("^",names(labels)[i], "(.*)", sep = ""),
                                           "\\1",
                                           nms[idx])
                } else {
                    new_nms[idx] <- gsub(paste("^",names(labels)[i], "(.*)", sep = ""),
                                         paste(labels[i], sep, "\\1", sep = ""),
                                         nms[idx])
                }
            }
            nms[idx] <- ""
        }
    }
    object[, 1] <- new_nms
    rownames(object) <- NULL
    object
}



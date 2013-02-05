################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

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

    if (!is.na(wchPval)) {
        for (i in names(xtTab)[-wchPval]) {
            xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                                 scientific = scientific, ...)
        }
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        xtTab[, wchPval] <- format.pval(round(xtTab[, wchPval], digits = r.digits),
                                        digits = digits, scientific = scientific,
                                        eps = smallest.pval, ...)
    } else {
        for (i in names(xtTab)) {
            xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                                 scientific = scientific, ...)
        }
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
    x <- lme4::summary(object)
    xtTab <- as.data.frame(x@coefs)
    wchPval <- match("Pr(>|z|)", names(xtTab))

    if (!is.na(wchPval)) {
        for (i in names(xtTab)[-wchPval]) {
            xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                                 scientific = scientific, ...)
        }
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        xtTab[, wchPval] <- format.pval(round(xtTab[, wchPval], digits = r.digits),
                                        digits = digits, scientific = scientific,
                                        eps = smallest.pval, ...)
    } else {
        for (i in names(xtTab)) {
            xtTab[, i] <- format(zapsmall(xtTab[, i]), digits = digits,
                                 scientific = scientific, ...)
        }
        warning("No p-value detected.")
    }
    row.names(xtTab) <- dimnames(x@coefs)[[1]]
    xtTab
}

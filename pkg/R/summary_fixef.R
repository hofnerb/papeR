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
                              smallest.pval = 0.001, ci = TRUE, ...){
    x <- summary(object)
    xtTab <- as.data.frame(x$tTable)
    if (ci) {
        xtTab <- cbind(xtTab, confint(object))
        nc <- ncol(xtTab)
        xtTab <- xtTab[, c(1, (nc - 1):nc, 2:(nc - 2))]
    }
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
                              smallest.pval = 0.001, ci = TRUE, ...){
    x <- lme4::summary(object)
    xtTab <- as.data.frame(x@coefs)
    if (ci) {
        xtTab <- cbind(xtTab, confint(object))
        nc <- ncol(xtTab)
        xtTab <- xtTab[, c(1, (nc - 1):nc, 2:(nc - 2))]
    }
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

################################################################################

#' Confidence intervals for mixed models
#'
#' Compute confidence intervals for mixed models from packages \pkg{nlme} and
#' \pkg{lme4}
#'
#' @param object Model of class \code{lme} or \code{mer}.
#' @param parm Parameters to be included in the confidence interval. See
#' \code{\link{confint.default}} for details.
#' @param level the confidence level.
#' @param ... Additional arguments. Currently not used.
#' @return Matrix with confidence intervalls.
#' @author Benjamin Hofner
#'
#' @rdname confint
#'
#' @method confint lme
#' @S3method confint lme
confint.lme <- function (object, parm, level = 0.95, ...) {
    cf <- fixef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    df <- summary(object)$tTable[parm, "DF"]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac_low <- qt(a[1], df)
    fac_high <- qt(a[2], df)
    fac <- cbind(fac_low, fac_high)
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses * fac
    ci
}

#' @rdname confint
#'
#' @method confint mer
#' @S3method confint mer
confint.mer <- function (object, parm, level = 0.95, ...) {

    tab <- as.data.frame(lme4::summary(object)@coefs)
    wchZval <- match("z value", names(tab))
    if (is.na(wchZval))
        stop("Currently only ", sQuote("mer"), " models with ",
             sQuote("z values"), " are supported.\n",
             "Try function ci() from package gmodels instead.")

    cf <- fixef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(parm, pct))
    ses <- sqrt(diag(as.matrix(vcov(object))))
    names(ses) <- pnames
    ses <- ses[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}

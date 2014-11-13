
################################################################################
# sapply function that differentiates between data.frames and (numeric) vectors
mySapply <- function(object, FUN, ...)
    UseMethod("mySapply")

mySapply.data.frame <- function(object, FUN, ...) {
    sapply(object, FUN, ...)
}

mySapply.default <- function(object, FUN, ...) {
    FUN(object, ...)
}


################################################################################
# marginal anova function in the fashion of library(car) for mixed models
Anova.lme <- function(mod, type = c("marginal", "sequential"), ...) {
    type <- match.arg(type)
    nlme::anova.lme(mod, type = type, ...)
}

################################################################################
# add and get options from tables
add_options <- function(object, ..., class) {
    attr(object, "latex.table.options") <- list(...)
    class(object) <- c(class, class(object))
    object
}

get_options <- function(object, name) {
    attr(object, "latex.table.options")[[name]]
}


## modified version based on confint.lm from package stats
##
## Copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
## Copyright (C) 2003-2012 The R Core Team
## URL: http://cran.at.r-project.org/src/base/R-3/R-3.0.1.tar.gz
## Inside archive path: /src/library/stats/R/confint.R
## Licence of R package utils: >= GPL-2
confint.lme <- function (object, parm, level = 0.95, ...) {
    cf <- nlme::fixef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    df <- summary(object)$tTable[parm, "DF"]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac_low <- qt(a[1], df)
    fac_high <- qt(a[2], df)
    fac <- cbind(fac_low, fac_high)
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses * fac
    ci
}


## function for lme4 version < 1.0 only
confint.mer <- function (object, parm, level = 0.95,
                         simulate = c("ifneeded", TRUE, FALSE),
                         B = 1000, ...) {

    simulate <- as.character(simulate)
    simulate <- match.arg(simulate)
    #tab <- as.data.frame(lme4::summary(object)@coefs)
    tab <- as.data.frame(summary(object)@coefs)
    wchZval <- match("z value", names(tab))
    if (is.na(wchZval) && simulate == "FALSE")
        warning("Currently only asymptotic confidence intervals for ", sQuote("mer"), " models with ",
                sQuote("z values"), " are supported.\n",
                "Use simulated confidence intervals instead.")

    if (simulate == "TRUE" || (is.na(wchZval) && simulate == "ifneeded")) {
        ## use ci() from package gmodels
        CI <- ci(x = object, confidence = level, n.sim = B)
        ## extract conifidence intervals
        CI <- CI[parm, 2:3, drop = FALSE]
        return(CI)
    }

    cf <- lme4::fixef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(parm, pct))
    ses <- sqrt(diag(as.matrix(vcov(object))))
    names(ses) <- pnames
    ses <- ses[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}


refit_model <- function(cl, ENV = globalenv(), summary, .call = "prettify") {

    if (!is.null(cl[["data"]]) && is.name(cl[["data"]]) &&
        is.null(ENV[[as.character(cl[["data"]])]])) {

        return(FALSE)  ## set confint = FALSE
        ## else: data might be a data.frame
    }
    if (is.null(cl[["data"]]) &&
        any(sapply(all.vars(cl[["formula"]]),
                   function(what) is.null(ENV[[what]])))) {

        return(FALSE)  ## set confint = FALSE
    }
    mod <- eval(cl, envir = ENV)
    ## needed to really call summary from lme4 (< 1.0)
    if (class(mod) == "mer") {
        # ae <- all.equal(lme4::summary(mod), summary)
        ae <- all.equal(summary(mod), summary)
    } else {
        ae <- all.equal(summary(mod), summary)
    }

    if (!all(ae == TRUE))
        warning(" In ", .call, ":\n",
                "  Summary specified via argument ", sQuote("object"),
                " and summary of refitted model differ.\n",
                "  Make shure that the data set has not been changed.\n",
                "  Differences are:\n",
                paste("  ", ae, "\n"), call. = FALSE)

    return(mod)
}

## modified version based on format.perc from package stats
##
## Copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
## Copyright (C) 2003-2012 The R Core Team
## URL: http://cran.at.r-project.org/src/base/R-3/R-3.0.1.tar.gz
## Inside archive path: /src/library/stats/R/confint.R
## Licence of R package utils: >= GPL-2
## Author of the original function: Martin Maechler
format.perc <- function(probs, digits) {
    txt <- format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits)
    paste(txt, "%")
}

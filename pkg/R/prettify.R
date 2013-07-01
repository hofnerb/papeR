################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# Prettify function for summary tables
prettify <- function(object, ...)
    UseMethod("prettify")

## function(object, digits = NULL, scientific = FALSE,
##          smallest.pval = 0.001, ci = TRUE, level = 0.95)
prettify.summary.lm <- function(object, confint = TRUE, level = 0.95,
                                smallest.pval = 0.001, digits = NULL, scientific = FALSE,...) {

    res <- as.data.frame(object$coefficients)
    if (confint){
        mod <- eval(object$call, envir = attr(object$terms, ".Environment"))
        CI <- confint(mod, level = level)
        res$CI_lower <- CI[,1]
        res$CI_upper <- CI[,2]
        ## move confint to the front
        newVars <- (ncol(res) -1):ncol(res)
        res <- cbind(res[, 1, drop = FALSE],
                     res[, newVars],
                     res[, - c(1, newVars)])
        names(res)[2] <- "CI (lower)"
        names(res)[3] <- "CI (upper)"
    }

    wchPval <- grep("Pr(>|.*|)", names(res))
    if (!is.na(wchPval)) {
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        res[, wchPval] <- format.pval(round(res[, wchPval], digits = r.digits),
                                      digits = digits, scientific = scientific,
                                      eps = smallest.pval, ...)
    } else {
        warning("No p-value detected.")
    }

    prettify(res, ...)
}

prettify.summary.glm <- function(object,
                                 confint = TRUE, level = 0.95, OR = TRUE,
                                 smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                                 signif.stars = getOption("show.signif.stars"), ...) {
    if (object$family$family == "binomial" && OR) {
        OR <- TRUE
    } else {
        OR <- FALSE
    }
    res <- as.data.frame(object$coefficients)
    if (OR) {
        res$OR <- exp(res$Estimate)
    }
    if (confint){
        mod <- eval(object$call, envir = attr(object$terms, ".Environment"))
        CI <- confint(mod, level = level)
        if (OR) {
            res$CI_lower <- exp(CI[,1])
            res$CI_upper <- exp(CI[,2])
            ## move confint to the front
            newVars <- (ncol(res) - 2):ncol(res)
            res <- cbind(res[, 1, drop = FALSE],
                         res[, newVars],
                         res[, - c(1, newVars)])
            names(res)[2] <- "Odds Ratio"
            names(res)[3] <- "CI (lower)"
            names(res)[4] <- "CI (upper)"
        } else {
            res$CI_lower <- CI[,1]
            res$CI_upper <- CI[,2]
            ## move confint to the front
            newVars <- (ncol(res) -1):ncol(res)
            res <- cbind(res[, 1, drop = FALSE],
                         res[, newVars],
                         res[, - c(1, newVars)])
            names(res)[2] <- "CI (lower)"
            names(res)[3] <- "CI (upper)"
        }
    }
    wchPval <- grep("Pr(>|.*|)", names(res))
    if (signif.stars) {
        res$signif <- symnum(res[, wchPval], corr = FALSE, na = FALSE,
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("***", "**", "*", ".", " "))
        names(res)[names(res) == "signif"] <- "   "
    }
    if (!is.na(wchPval)) {
        r.digits <- 10
        num <- strsplit(as.character(smallest.pval), "\\.")[[1]]
        if (!is.null(num[2]))
            r.digits <- nchar(num[2])
        res[, wchPval] <- format.pval(round(res[, wchPval], digits = r.digits),
                                      digits = digits, scientific = scientific,
                                      eps = smallest.pval, ...)
    } else {
        warning("No p-value detected.")
    }

    prettify(res, ...)
}

prettify.data.frame <- function(object, labels, sep = ": ", extra.column = FALSE, ...) {
    ## get row names
    nms <- new_nms <- rownames(object)

    ## order labels to avoid matching with substrings
    labels <- labels[rev(order(sapply(names(labels), nchar)))]

    ## make extra column for factor levels if needed
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
    } else {
        object$varlabel <- " "
        newVars <- ncol(object)
        object <- cbind(object[, newVars],
                        object[, - newVars])
        names(object)[1] <- " "
        object[,1] <- as.character(object[,1])
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

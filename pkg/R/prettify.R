################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# Prettify function for summary tables
prettify <- function(object, ...)
    UseMethod("prettify")

## function(object, digits = NULL, scientific = FALSE,
##          smallest.pval = 0.001, ci = TRUE, level = 0.95)
prettify.summary.lm <- function(object, confint = FALSE, ...) {

    res <- as.data.frame(object$coefficients)
    if (confint){
        mod <- eval(object$call, envir = attr(object$terms, ".Environment"))
        CI <- confint(mod)
        res$CI_lower <- CI[,1]
        res$CI_upper <- CI[,2]
    }
    prettify(res, ...)
}

prettify.summary.glm <- function(object, confint = FALSE, OR = NULL, ...) {
    if (object$family$family == "binomial" && (is.null(OR) || OR)) {
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
        CI <- confint(mod)
        if (OR) {
            res$CI_lower <- exp(CI[,1])
            res$CI_upper <- exp(CI[,2])
        } else {
            res$CI_lower <- CI[,1]
            res$CI_upper <- CI[,2]
        }
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

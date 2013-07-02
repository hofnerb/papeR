################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# Extract labels from data sets
labels.data.frame <- function(object, which = NULL, abbreviate = FALSE, ...){
    ## if no labels specified set temporarily names as labels
    if (is.null(attr(object, "variable.labels")))
        labels(object) <- colnames(object)

    object <- CLEAN_LABELS(object)

    if (is.null(which)) {
        RET <- attr(object, "variable.labels")
    } else {
        if (is.numeric(which) && any(which > length(attr(object, "variable.labels"))) ||
            is.character(which) && !all(which %in% names(attr(object, "variable.labels"))))
            stop("One cannot extract labels for non-existing variables.")
        RET <- attr(object, "variable.labels")[which]
    }

    if (abbreviate) {
        nms <- names(RET)
        RET <- abbreviate(RET, ...)
        names(RET) <- nms
    }
    return(RET)
}

################################################################################
# Sets labels as attribute "variable.labels"
"labels<-" <- function(data, which = NULL, value){

    data <- CLEAN_LABELS(data)
    if (is.null(which)) {
        if (!is.null(value) && ncol(data) != length(value))
            stop("One must supply a label for each column of the data set\n",
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
    if (is.numeric(which) && any(which > ncol(data)))
        stop("One  cannot supply labels for none-existing variables")
    if (is.character(which) && !all(which %in% colnames(data))) {
        txt <- paste("One  cannot supply labels for none-existing variables\n",
                     "  Variables not found in data set:\n\t",
                     paste(which[!(which %in% colnames(data))],
                           collapse = "\n\t"))
        stop(txt)
    }

    if (length(which) != length(value))
        stop("One must supply a label for each selected column of the data set.")
    ## only set values which are not NA
    attr(data, "variable.labels")[which][!is.na(value)] <- value[!is.na(value)]
    return(data)
}

"labels[<-" <- function(data, i, value){
    labels(data, which = i) <- value
}


CLEAN_LABELS <- function(data) {
    ## drop spare labels
    spare <- !(names(attr(data, "variable.labels")) %in% names(data))
    if (any(spare)) {
        message("Note: A variable has been removed or renamed. ",
                "Corresponding variable labels are removed.")
        attr(data, "variable.labels") <-  attr(data, "variable.labels")[!spare]
    }
    ## add missing labels
    missing <- !(names(data) %in% names(attr(data, "variable.labels")))
    if (any(missing)) {
        tmp <- names(data)[missing]
        names(tmp) <- names(data)[missing]
        attr(data, "variable.labels") <- c(attr(data, "variable.labels"),
                                           tmp)
    }
    ## re-order
    attr(data, "variable.labels") <- attr(data, "variable.labels")[names(data)]
    ## return altered data set
    return(data)
}

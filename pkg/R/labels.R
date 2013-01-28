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

################################################################################
##  Author: Benjamin Hofner, benjamin.hofner@fau.de

################################################################################
# Extract labels from data sets
labels.labeled.data.frame <- function(object, which = NULL,
                                      abbreviate = FALSE, ...) {

    which <- check_which(which, object, "extract")

    RET <- sapply(object[which], get_labels)
    if (is.list(RET) && any(idx_null <- sapply(RET, is.null))) {
        RET[idx_null] <- colnames(object)[idx_null]
        RET <- unlist(RET)
    }

    if (abbreviate) {
        nms <- names(RET)
        RET <- abbreviate(RET, ...)
        names(RET) <- nms
    }
    return(RET)
}

######
#####  FIXME: What happens if we use labels(data[, 1:3]) <- 1:3
######


labels.data.frame <- function(object, which = NULL, abbreviate = FALSE, ...) {
    ## if no labels specified temporarily set names as labels
    if (is.null(attr(object, "variable.labels"))) {
        labels(object) <- colnames(object)
    } else {
        ## clean labels
        object <- CLEAN_LABELS(object)
        ## set these labels temporarily as elementwise labels
        labels(object) <- attr(object, "variable.labels")
    }

    ## now use labels.labeled.data.frame
    return(labels(object, which = which, abbreviate = abbreviate, ...))
}

labels.default <- function(object, ...){
    if (is.null(attr(object, "variable.label")))
        return(NULL)

    return(attr(object, "variable.label"))
}

################################################################################
# Sets labels
"labels<-" <- function(data, which = NULL, value){

    which <- check_which(which, data, "define")

    if (!is.null(value)) {
        if (length(which) != length(value))
            stop("One must supply a label for each _selected_ column of the data set.")
        if (is.character(which))
            names(value) <- which
    }

    for (i in which)
        attr(data[[i]], "variable.label") <- value[[i]]

    ## remove attribute of data set if it exists
    if (!is.null(attr(data, "variable.labels")))
        attr(data, "variable.labels") <- NULL

    if (!("labeled.data.frame" %in% class(data)))
        class(data) <- c("labeled.data.frame", class(data))
    return(data)
}

"labels[<-" <- function(data, i, value)
    labels(data, which = i) <- value


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


## define coercion function
as.labeled.data.frame <- as.labelled.data.frame <- function(object, ...)
    UseMethod("as.labeled.data.frame")

as.labeled.data.frame.data.frame <- function(object, ...) {
    labels(object) <- labels(object)
    object
}

is.labeled.data.frame <- is.labelled.data.frame <- function(object)
    "labeled.data.frame" %in% class(object)

## ## special extraction function that copies the relevant labels
## "[.labeled.data.frame" <- function(x, ..., drop = TRUE) {
##     lbls <- labels(x)
##     x <- NextMethod("[", drop = drop)
##     if (!is.null(dim(x)) || !drop)
##         labels(x) <- lbls[names(x)]
##     x
## }
##
## ## special subset function that copies the relevant labels
## subset.labeled.data.frame <- function(x, ...) {
##     lbls <- labels(x)
##     x <- subset.data.frame(x, ...)
##     labels(x) <- lbls[names(x)]
##     x
## }
##
##
## ## special cbind function that copies the relevant labels
## cbind.labeled.data.frame <- function(..., deparse.level = 1) {
##     objects <- list(...)
##     if (any(!(which.df <- sapply(objects, is.data.frame))))
##         warning("Not all objects are data.frames, some labels might be wrong")
##     lbls <- unlist(lapply(objects[which.df], labels))
##     x <- cbind.data.frame(...,  deparse.level = deparse.level)
##     labels(x) <- lbls[names(x)]
##     x
## }

check_equality <- function(x, y) {
    ## exactly equal
    if (all(x == y)) {
        return(TRUE)
    }
    ## unequal length
    if (length(x) != length(y)) {
        return(FALSE)
    }
    ## equal but different order
    anywhere <- rep(NA, length(x))
    for (i in 1:length(x))
        anywhere[i] <- x[i] %in% y
    if (all(anywhere)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


## special cbind function that copies the relevant labels
rbind.labeled.data.frame <- function(..., deparse.level = 1) {
    nms <- lapply(list(...), names)
    lbls <- lapply(list(...), labels)
    diff_lbls <- FALSE
    diff_nms <- FALSE
    for (i in 2:length(nms)) {
        diff_nms <- any(!check_equality(nms[[i]], nms[[1]]), diff_nms)
    }
    if (diff_nms)
        stop("names of 'data.frame's differ")

    for (i in 2:length(lbls)) {
        diff_lbls <- any(!check_equality(lbls[[i]], lbls[[1]]), diff_lbls)
    }
    if (diff_lbls)
        warning("Labels differ; Labels of first data.set are used.")
    lbls <- lbls[[1]]
    x <- rbind.data.frame(..., deparse.level = deparse.level)
    labels(x) <- lbls
    x
}

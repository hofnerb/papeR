## sapply function that differentiates between data.frames and (numeric) vectors

mySapply <- function(object, FUN, ...)
    UseMethod("mySapply")

mySapply.data.frame <- function(object, FUN, ...) {
    sapply(object, FUN, ...)
}

mySapply.numeric <- function(object, FUN, ...) {
    FUN(object, ...)
}

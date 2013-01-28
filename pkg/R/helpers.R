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
    nlme:::anova.lme(mod, type = type, ...)
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

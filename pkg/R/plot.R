plot.labeled.data.frame <- function(data, variables = names(data),
                                    labels = TRUE, by = NULL,
                                    with = NULL, regression.line = TRUE,
                                    line.col = "red", ...) {

    if (is.numeric(variables)) {
        variables <- names(data)[variables]
    }

    if (!is.null(with)) {
        if (!is.null(by))
            stop("One can only specify either ", sQuote("by"), " or ",
                 sQuote("with"))
        by <- with
    }

    if (is.numeric(by)) {
        by <- names(data)[by]
    }

    if (!all(c(by, variables) %in% names(data)))
        stop("(Some of the) specified variables are not available in data")

    ## set up labels
    if (is.null(labels)) {
        labels <- variables
    } else {
        if (is.logical(labels) && labels) {
            labels <- labels(data, which = variables)
            if (!is.null(by))
                grp_label <- labels(data, which = by)
        } else {
            if (length(variables) != length(labels))
                stop(sQuote("variables"), " and ", sQuote("labels"),
                     " must have the same length")
        }
    }

    if (!is.null(by)) {
        if(!is.factor(data[, by]) && !is.numeric(data[, by]))
            stop(sQuote("by"), " must specify a factor or numeric variable")
        if (by %in% variables) {
            idx <- variables != by
            variables <- variables[idx]
            labels <- labels[idx]
        }
        by_var <- data[, by]
    }

    data <- data[, variables, drop = FALSE]

    ## get numerical variables
    num <- mySapply(data, is.numeric)
    fac <- mySapply(data, is.factor)

    ## if anything else is present (not num or fac)
    if (!all(num | fac))
        warning("Only numeric or factor variables are plotted")

    which.num <- which(num)
    which.fac <- which(fac)

    if (is.null(by)) {
        for (i in which.num) {
            boxplot(data[, i], main = variables[i], ylab = labels[i], ...)
        }
        for (i in which.fac) {
            barplot(table(data[, i]), main = variables[i], ylab = labels[i], ...)
        }
    } else {
        grp_label <- ifelse(!is.null(grp_label), grp_label, by)
        if (is.factor(by_var)) {
            for (i in which.num) {
                cc <- complete.cases(data[, i], by_var)
                tmp_by_var <- by_var[cc, drop = TRUE]
                boxplot(data[cc, i] ~ tmp_by_var, main = variables[i],
                        ylab = labels[i], xlab = grp_label, ...)
            }
            for (i in which.fac) {
                cc <- complete.cases(data[, i], by_var)
                tmp_by_var <- by_var[cc, drop = TRUE]
                plot(data[cc, i] ~ tmp_by_var, main = variables[i],
                     ylab = labels[i], xlab = grp_label, ...)
            }
        } else {  ## i.e. is.numeric(by_var)
            for (i in which.num) {
                cc <- complete.cases(data[, i], by_var)
                tmp_by_var <- by_var[cc, drop = TRUE]
                plot(tmp_by_var ~ data[cc, i], main = variables[i],
                     xlab = labels[i], ylab = grp_label, ...)
                if (regression.line)
                    abline(lm(tmp_by_var ~ data[cc, i]), col = line.col)
            }
            for (i in which.fac) {
                cc <- complete.cases(data[, i], by_var)
                tmp_by_var <- by_var[cc, drop = TRUE]
                boxplot(tmp_by_var ~ data[cc, i], main = variables[i],
                        xlab = labels[i], ylab = grp_label, ...)
            }
        }
    }
}

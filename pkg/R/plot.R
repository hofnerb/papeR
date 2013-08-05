plot.labeled.data.frame <- function(data, variables = names(data),
                                    labels = TRUE, group = NULL, ...) {

    if (is.numeric(variables)) {
        variables <- names(data)[variables]
    }

    if (is.numeric(group)) {
        group <- names(data)[group]
    }

    ## set up labels
    if (is.null(labels)) {
        labels <- variables
    } else {
        if (is.logical(labels) && labels) {
            labels <- labels(data, which = variables)
            if (!is.null(group))
                grp_labels <- labels(data, which = group)
        } else {
            if (length(variables) != length(labels))
                stop(sQuote("variables"), " and ", sQuote("labels"),
                     " must have the same length")
        }
    }

    if (!is.null(group)) {
        if(!is.factor(data[, group]))
            stop(sQuote("group"), " must be a factor variable")
        if (group %in% variables) {
            idx <- variables != group
            variables <- variables[idx]
            labels <- labels[idx]
        }
        group_var <- data[, group]
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

    if (is.null(group)) {
        for (i in which.num) {
            boxplot(data[, i], main = variables[i], ylab = labels[i], ...)
        }
        for (i in which.fac) {
            barplot(table(data[, i]), main = variables[i], ylab = labels[i], ...)
        }
    } else {
        grp_labels <- ifelse(!is.null(grp_labels), grp_labels, group)
        for (i in which.num) {
            cc <- complete.cases(data[, i], group_var)
            tmp_group_var <- group_var[cc, drop = TRUE]
            boxplot(data[, i] ~ tmp_group_var, main = variables[i],
                    ylab = labels[i], xlab = grp_labels, ...)
        }
        for (i in which.fac) {
            cc <- complete.cases(data[, i], group_var)
            tmp_group_var <- group_var[cc, drop = TRUE]
            plot(data[cc, i] ~ tmp_group_var, main = variables[i],
                 ylab = labels[i], xlab = grp_labels, ...)
        }
    }
}

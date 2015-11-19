library("papeR")
context("summarize functions")

if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

############################################################
## test old functions latex.table.fac / latex.table.cont
############################################################

test_that("latex.table.cont works", {
    expect_output(latex.table.cont(Orthodont),
                  paste0("tabular.*",
                         "& N &   & Mean & SD &   & Min & Q1 & Median & Q3 & Max",
                         ".*",
                         "distance & 108 &  & 24.02 & 2.93 &  & 16.50 & 22.00 & 23.75 & 26.00 & 31.50",
                         ".*",
                         "age & 108 &  & 11.00 & 2.25 &  & 8.00 & 9.00 & 11.00 & 13.00 & 14.00"))
    ## check that longtable isn't printed here
    expect_that(latex.table.cont(Orthodont), not(prints_text("longtable")))
    ## but here
    expect_output(latex.table.cont(Orthodont, table = "longtable"), "longtable")
})

test_that("latex.table.fac works", {
    expect_output(latex.table.fac(Orthodont),
                  paste0("tabular.*",
                         "& Level &   & N & \\\\%",
                         ".*",
                         "Subject & M16 &  &   4 & 3.7",
                         ".*",
                         "Sex & Male &  &  64 & 59.3",
                         ".*",
                         "& Female &  &  44 & 40.7"))
    ## check that longtable isn't printed here
    expect_that(latex.table.fac(Orthodont), not(prints_text("longtable")))
    ## but here
    expect_output(latex.table.fac(Orthodont, table = "longtable"), "longtable")
})

############################################################
## test variable labels in summaries
############################################################

test_that("variable labels work", {
    factor <- sapply(Orthodont, is.factor)
    for (type in c("numeric", "factor")) {
        data <- Orthodont
        labels(data) <- c("Distance (mm)", "Age (yr)", "ID", "Sex")
        which <- if (type  == "numeric") { !factor } else { factor }

        ## summary with data set labels
        summary <- summarize(data, type = type, variable.labels = TRUE)
        expect_equivalent(summary[summary[, 1] != "", 1],
                          labels(data)[which],
                          info = type)

        ## summary with user specified labels
        usr_labels <- summarize(data, type = type,
                                variable.labels = c("a", "b", "c", "d"))
        expect_equivalent(usr_labels[usr_labels[, 1] != "", 1],
                          c("a", "b", "c", "d")[which],
                          info = type)

        ## grouped summary with data set labels
        which[4] <- FALSE
        grouped <- summarize(data, type = type, group = "Sex",
                             variable.labels = TRUE)
        expect_equivalent(grouped[grouped[, 1] != "", 1],
                          labels(data)[which],
                          info = type)
    }
})

test_that("grouped summaries work", {
    ## grouped summaries for numerics
    numeric <- summarize(Orthodont, type = "numeric", group = "Sex")
    expect_equivalent(numeric[, 2], rep(levels(Orthodont$Sex), 2))
    expect_equivalent(numeric$p.value[c(1,3)], c("<0.001", "1.000"))
    ## grouped summaries for factors
    factor <- summarize(Orthodont, type = "factor", group = "Sex")
    expect_equivalent(factor[, 2], levels(Orthodont$Subject))
    expect_equivalent(ncol(factor), 10)
    expect_equivalent(factor$p.value[1], "< 0.001")
})

## FIX GROUP_LABELS

}

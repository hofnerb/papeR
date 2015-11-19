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
        summary <- summarize(data, type = type, variable.labels = TRUE)
        expect_equivalent(summary[summary[,1] != "",1],
                          labels(data)[which],
                          info = type)
    }
})

## ADD TEST FOR USER SPECIFIED LABELS


test_that("grouped summaries work", {
    for (type in c("numeric", "factor")) {
        expect_that(summarize(Orthodont, type = type, group = "Sex"),
                    not(throws_error()), info = type)

        #which <- if (type  == "numeric") { !factor } else { factor }
        #expect_that(summarize(Orthodont, type = type, variable.labels = TRUE,
        #                      group = "Sex"), not(throws_error()), info = type)
        #expect_equivalent(summary[summary[,1] != "",1],
        #                  labels(data)[which],
        #                  info = type)
   }
})

## FIX GROUP_LABELS

}


## summarize(Orthodont, type = "numeric", group = "Sex")
## debugonce(summarize_factor)
## summarize(Orthodont, type = "factor", group = "Sex")

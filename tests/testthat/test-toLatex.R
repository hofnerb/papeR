library("papeR")
context("toLatex")

############################################################
## toLatex.default
############################################################

test_that("toLatex.default is correctly used", {
    library("xtable")
    x <- matrix(1:3)
    y <- toLatex(xtable(x))
    expect_equal(class(y), "Latex")
    expect_equal(y[c(3, 14)], c("\\begin{table}[ht]", "\\end{table}"))
})

############################################################
## toLatex.character
############################################################

test_that("toLatex.character works", {
    expect_equal(toLatex("a"), "a")
    expect_equal(toLatex("$\\sum$"), "$\\sum$")
    expect_equal(toLatex("\\"), "$\\backslash$")
    expect_equal(toLatex("$"), "\\$")
    expect_equal(toLatex(">="), "$\\geq$")
    expect_equal(toLatex("<="), "$\\leq$")
    expect_equal(toLatex("<"), "$<$")
    expect_equal(toLatex(">"), "$>$")
    expect_equal(toLatex("|"), "$|$")
    expect_equal(toLatex("{"), "\\{")
    expect_equal(toLatex("}"), "\\}")
    expect_equal(toLatex("%"), "\\%")
    expect_equal(toLatex("&"), "\\&")
    expect_equal(toLatex("_"), "\\_")
    expect_equal(toLatex("#"), "\\#")
    expect_equal(toLatex("a^1"), "a$^{1}$")
    expect_equal(toLatex("a^(1)"), "a\\verb|^|(1)")
    expect_equal(toLatex("~"), "\\~{}")
    expect_equal(toLatex("²"), "$^2$")
    expect_equal(toLatex("³"), "$^3$")
})

############################################################
## toLatex.sessionInfo
############################################################

test_that("toLatex.sessionInfo is correctly used", {
    expect_message(a <- papeR::toLatex(sessionInfo()),
                   "Written .* BibTeX entries to file 'Rpackages.bib' ...\n.*")
    expect_message(a <- papeR::toLatex(sessionInfo(), file = "bib.bib"),
                   "Written .* BibTeX entries to file 'bib.bib' ...\n.*")
    expect_equal(class(a), "Latex")
    expect_true(any(grepl("\\citep", a)))

    ## expect no message when file is NULL
    expect_message(a <- papeR::toLatex(sessionInfo(), file = NULL), NA)
    expect_equal(class(a), c("LatexBibtex", "Latex"))
    expect_false(is.null(attr(a, "BibTeX")))
    expect_true(any(grepl("\\citep", a)))

    expect_message(a <- papeR::toLatex(sessionInfo(), file = NULL, citations = FALSE), NA)
    expect_equal(class(a), "Latex")
    expect_false(is.null(attr(a, "BibTeX")))
    expect_false(any(grepl("\\citep", a)))

    expect_message(a <- papeR::toLatex(sessionInfo(), citations = FALSE), NA)
    expect_equal(class(a), "Latex")
    expect_false(any(grepl("\\citep", a)))

    a <- papeR::toLatex(sessionInfo(), pkgs = "xtable", file = NULL)
    expect_match(a[5], "xtable")
    expect_equal(length(a), 7)

    expect_warning(a <- papeR::toLatex(sessionInfo(), pkgs = "xtable",
                                       other.pkgs = FALSE, file = NULL),
                   "should be TRUE")
    expect_equal(length(a), 3)
})


############################################################
## print.LatexBibtex
############################################################

############################################################
## toLatex.LatexBibtex
############################################################

############################################################
## toBibtex.LatexBibtex
############################################################

############################################################
## write.bib
############################################################

library("papeR")
context("toLatex")

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
    expect_equal(toLatex("\u00B2"), "$^2$")
    expect_equal(toLatex("\u00B3"), "$^3$")
})

############################################################
## toLatex.sessionInfo
############################################################

test_that("toLatex.sessionInfo is correctly used", {
    expect_message(a <- toLatex(sessionInfo(), file = "bib.bib"),
                   "Written .* BibTeX entries to file 'bib.bib' ...\n.*")
    expect_equal(class(a), "Latex")
    expect_true(any(grepl("\\citep", a)))

    ## expect no message when file is NULL
    expect_message(b <- toLatex(sessionInfo()), NA)
    expect_equal(class(b), c("LatexBibtex", "Latex"))
    expect_false(is.null(attr(b, "BibTeX")))
    expect_true(any(grepl("\\citep", b)))

    expect_message(c <- toLatex(sessionInfo(), citations = FALSE), NA)
    expect_equal(class(c), "Latex")
    expect_true(is.null(attr(c, "BibTeX")))
    expect_false(any(grepl("\\citep", c)))

    expect_message(d <- toLatex(sessionInfo(), citations = FALSE,
                                file = "bib.bib"), NA)
    expect_equal(class(d), "Latex")
    expect_false(any(grepl("\\citep", d)))

    e <- toLatex(sessionInfo(), pkgs = "xtable")
    expect_match(e[5], "xtable")
    expect_equal(length(e), 7)

    expect_warning(e <- toLatex(sessionInfo(), pkgs = "xtable",
                                other.pkgs = FALSE),
                   "should be TRUE")
    expect_equal(length(e), 3)

    expect_match(toLatex(sessionInfo(), locale =  TRUE)[3], "Locale:")
    expect_match(toLatex(sessionInfo(), base.pkgs =  TRUE)[3], "Base packages:")
    expect_true(any(grepl(".*namespace.*", toLatex(sessionInfo(), namespace.pkgs = TRUE))))
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

test_that("write.bib", {
    expect_message(write.bib(c()),
                   "Empty package list: nothing to be done.")
    expect_error(write.bib("nonexisting_pkg"),
                 "package .* not found")
    rref <- bibentry(
        bibtype = "Manual",
        title = "R: A Language and Environment for Statistical Computing",
        author = person("R Core Team"),
        organization = "R Foundation for Statistical Computing",
        address = "Vienna, Austria",
        year = 2014,
        url = "http://www.R-project.org/")
    expect_equal(write.bib(rref), rref)
    expect_error(write.bib(1), "Invalid argument")
})

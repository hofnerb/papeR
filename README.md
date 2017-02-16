papeR
=====

[![Build Status (Linux)](https://travis-ci.org/hofnerb/papeR.svg?branch=master)](https://travis-ci.org/hofnerb/papeR)
[![Build Status (Windows)](https://ci.appveyor.com/api/projects/status/t58j1j2hygy6evst/branch/master?svg=true)](https://ci.appveyor.com/project/hofnerb/paper/branch/master)
[![Coverage Status](https://coveralls.io/repos/hofnerb/papeR/badge.svg?branch=master&service=github)](https://coveralls.io/github/hofnerb/papeR?branch=master)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/papeR)](https://cran.r-project.org/package=papeR)
[![](http://cranlogs.r-pkg.org/badges/papeR)](https://cran.r-project.org/package=papeR)

**papeR** provides a toolbox for writing knitr, Sweave or other LaTeX- or markdown-based papers and reports and to prettify the output of various estimated models.

## Installation:

- Current version (from CRAN):

```r
install.packages("papeR")
```

- Latest development version from GitHub:

```r
library("devtools")
install_github("hofnerb/papeR")
```

- To be able to use the `install_github()` command, one needs to install `devtools` first:

```r
install.packages("devtools")
```

## Using papeR

Tutorials on how to use **papeR** can be found on CRAN:

- [Using papeR with Markdown](https://cran.r-project.org/package=papeR/vignettes/papeR_introduction.html)
- [Using papeR with LaTeX](https://cran.r-project.org/package=papeR/vignettes/papeR_with_latex.pdf)

or within R via 

```r
## introduction to papeR (in combination with Markdown)
vignette("papeR_introduction", package = "papeR")
## introduction to papeR with LaTeX
vignette("papeR_with_latex", package = "papeR")
```
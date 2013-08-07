\name{toLatex}
\alias{toLatex.character}
\alias{toLatex.sessionInfo}

\title{ Cleaning R Code for printing in LaTeX environments }
\description{
  The function produces code that LaTeX is able to typeset.
}
\usage{
\method{toLatex}{character}(object, ...)

\method{toLatex}{sessionInfo}(object, pkgs = NULL, locale = FALSE,
        base.pkgs = FALSE, other.pkgs = TRUE,
        namespace.pks = FALSE, citations = TRUE,
        citecommand = "\\\\citep", file = "Rpackages.bib",
        append = FALSE, ...)
}

\arguments{
  \item{object}{
    either an object of class character which should be cleaned for printing in
    LaTeX environments or an object of class \code{\link{sessionInfo}}.
  }
  \item{pkgs}{
    character vector (optional). Specify specific packages here to show
    information on these (instead of all attached packages). See
    \code{package} in \code{\link{sessionInfo}}.
  }
  \item{locale}{
    logical (default = FALSE). Show information on locale.
  }
  \item{base.pkgs}{
    logical (default = FALSE). Show information on base packages.
  }
  \item{other.pkgs}{
    logical (default = TRUE). Show information on currently attached
    packages. If \code{pkgs} is specified, information on these
    packages is given instead of all attached packages.
  }
  \item{namespace.pks}{
    logical (default = FALSE). Show information on packages whose
    namespaces are currently loaded but not attached.
  }
  \item{citations}{
    logical (default = TRUE). Should citations for all packages be
    added? BibTeX is used for storing the citations.
  }
  \item{citecommand}{
    Specify LaTeX-command for citation here. Curly brackets are added
    internally. Note that \code{\\} needs to be escaped, i.e., one needs
      to write \code{\\\\} instead.
  }
  \item{file}{
    Specify path to BibTeX file where citations should be saved. If
    \code{file = NULL} is specified, the BibTeX entries are attached to
    the output as attribute \code{"BibTeX"}. See examples for details.
  }
  \item{append}{
    logical (default = FALSE). Should citations be added to an existing
    BibTeX file (if existing) or should old BibTeX files be overwritten?
  }
  \item{...}{
    additional arguments. Currently not used.
  }
}
\value{
  A character string with special markup is returned: The output is
  printed with LaTeX style syntax highlighting to be used
  e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner, based on code from package xtable, bibtex and package utils.
  See source code for documentation.
}
\seealso{
  \code{\link{toLatex}}. For details on \code{toLatex.sessionInfo} see
  also \code{\link{sessionInfo}}.
}
\examples{
txt <- "Price: <= 500$ & additional goodies"
toLatex(txt)

############################################################
## session info for automatic inclusion in reports

info <- toLatex(sessionInfo(), file = NULL)
info

## extract first part (the Latex part)
toLatex(info)
## extract second part (the BibTeX part)
toBibtex(info)


############################################################
## usual usage scenario

## Do not run automatically as it needs write access to the current
## working directory. This code (without removing the file) could
## for example be included in a Latex chunk of your Sweave or knitr
## report.

\dontrun{getwd()     ## location where write access is needed
toLatex(sessionInfo(), file = "packages.bib")
file.remove("packages.bib")
}

}

\keyword{univar}
\keyword{IO}
\keyword{print}

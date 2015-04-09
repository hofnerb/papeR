\name{latex.table.fac}
\alias{latex.table.fac}
\alias{print.table.fac}

\title{ Produce (LaTeX) Summaries for Factor Variables }
\description{
  The function produces LaTeX tables with summary statistics for factor
  variables. It makes use of the booktabs package in LaTeX to obtain
  tables with a nice layout.
}
\usage{
latex.table.fac(..., caption = NULL, label = NULL,
    table = c("tabular", "longtable"), align = NULL,
    floating = FALSE, center = TRUE)
}

\arguments{
  \item{...}{
    arguments for \code{\link{summarize}}. See there for details.
  }
  \item{caption}{
    (optional) character string. Caption of LaTeX table. Note that
    captions are suported for all tables (see also details below).
  }
  \item{label}{
    (optional) character string. Label of LaTeX table specified as
    \code{\\label{"label"}}.
  }
  \item{table}{
    character string. LaTeX table format, currently either
    \code{"tabular"} (default) or \code{"longtable"}.
  }
  \item{align}{
    character string. LaTeX alignment of table rows, per default
    \code{"lllr...r"}, where \code{"r"} is repeated \code{ncol - 2} times.
  }
  \item{floating}{
    logical (default: \code{FALSE}). Determines whether the table is a floating
    object (i.e. use a \code{table} environment or not). Note that a
    \code{longtable} cannot be a floating object but captions can be
    used.
  }
  \item{center}{
    logical (default: \code{TRUE}). Determines if table should be centered.
  }
}
\details{
  This function is deprecated and only available for backward
  comaptibility. Use \code{\link{summarize}} for more flexibility.

  The output requires \code{\\usepackage{booktabs}} in the LaTeX file.

  Captions can be added to both, \code{longtable}s and \code{tabular}s.
  In the latter case, captions are also suported if the table is no
  floating object. In this case, the LaTeX package \code{capt-of} is
  required.
}
\value{
  The output is printed with LaTeX style syntax highlighting to be used
  e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{latex.table.cont}} and \code{\link{get_option}}
}
\examples{
## Example requires package nlme to be installed and loaded
if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

    ## Get summary for continuous variables
    latex.table.fac(Orthodont)

    ## Reorder data for table:
    latex.table.fac(Orthodont, variables = c("Sex", "Subject"))

    ## What happens in the display if we introduce some missing values:
    set.seed(1907)
    Orthodont$Sex[sample(nrow(Orthodont), 20)] <- NA
    latex.table.fac(Orthodont)
    latex.table.fac(Orthodont, variables = "Sex")
    ## do not show statistics on missing values
    latex.table.fac(Orthodont, variables = "Sex", show.NAs = FALSE)
}
}

\keyword{univar}
\keyword{IO}
\keyword{print}

\name{latex.table.fac}
\alias{latex.table.fac}

\title{ Produce (LaTeX) Summaries for Factor Variables }
\description{
  The function produces LaTeX tables with summary statistics for factor
  variables. It makes use of the booktabs package in LaTeX to obtain
  tables with a nice layout.
}
\usage{

latex.table.fac(data, variables = names(data), colnames = NULL, digits = 2,
                table = c("tabular", "longtable"), align = NULL, sep = TRUE,
                drop = TRUE)
}

\arguments{
  \item{data}{
    data set to be used
  }
  \item{variables}{
    (factor) variables that should be included in the table. Per
    default, all factor variables of \code{data} are used.
  }
  \item{colnames}{
    a vector of character strings of length 4 (variable, label, N, and
    fraction in \%). The vector supplies alternative column names for
    the resulting LaTeX table. If \code{NULL} default names are used.
  }
  \item{digits}{
    number of digits to round to (only used for fractions).
  }
  \item{table}{
    character string. LaTeX table format, currently either
    \code{"tabular"} (default) or \code{"longtable"}.
  }
  \item{align}{
    character string. LaTeX alignment of table rows, per default
    \code{"llr...r"}, where \code{"r"} is repeated \code{ncol - 2} times.
  }
  \item{sep}{
    logical (default: TRUE). Determines wether separators (lines) should
    be added after each factor variable.
  }
  \item{drop}{
    logical (default: TRUE). Determines wether missing factor variables
    are dropped from the table.
  }
}
\details{
  The output requires \code{\\usepackage{booktabs}} in the LaTeX file.
}
\value{
  A \code{data.frame} with the computed statistics is returned
  (invisibly) and the output is printed with LaTeX style syntax
  highlighting to be used e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{latex.table.cont}}
}
\examples{

}

\keyword{univar}
\keyword{IO}
\keyword{print}

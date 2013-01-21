\name{latex.table.cont}
\alias{latex.table.cont}

\title{ Produce (LaTeX) Summaries for Continuous Variables }
\description{
  The function produces LaTeX tables with summary statistics for continous
  variables. It makes use of the booktabs package in LaTeX to obtain
  tables with a nice layout.
}
\usage{

latex.table.cont(data, variables = names(data), colnames = NULL, digits = 2,
                 table = c("tabular", "longtable"), align = NULL,
                 count = TRUE, mean_sd = TRUE, quantiles = TRUE,
                 incl_outliers = TRUE, drop = TRUE)

}

\arguments{
  \item{data}{
    data set to be used
  }
  \item{variables}{
    (continuous) variables that should be included in the table. Per
    default, all non-factor variables of \code{data} are used.
  }
  \item{colnames}{
    a vector of character strings of length 9 (variable, N, standard
    deviation, minimum, first quartile, median, third quartile and
    maximum) or shorter if some of \code{count}, \code{mean_sd} or
    \code{quantiles} is \code{FALSE}. The vector supplies alternative
    column names for the resulting LaTeX table. If \code{NULL} default
    names are used.
  }
  \item{digits}{
    number of digits to round to.
  }
  \item{table}{
    character string. LaTeX table format, currently either
    \code{"tabular"} (default) or \code{"longtable"}.
  }
  \item{align}{
    character string. LaTeX alignment of table rows, per default
    \code{"lr...r"}, where \code{"r"} is repeated \code{ncol - 1} times.
  }
  \item{count}{
    (logical) indicator if number of complete cases ("n") should be
    included in the table (default: TRUE).
  }
  \item{mean_sd}{
    (logical) indicator if mean and standard deviation should be
    included in the table (default: TRUE).
  }
  \item{quantiles}{
    (logical) indicator if quantiles (including min and max) should be
    included in the table (default: TRUE).
  }
  \item{incl_outliers}{
    Per default we use \code{\link{fivenum}} to compute the quantiles
    (if \code{quantiles = TRUE}). If extreme values should be excluded
    from min/max in the table, \code{boxplot( , plot = FALSE)$stats} is
    used instead.
  }
  \item{drop}{
    logical (default: TRUE). Determines wether missing continuous variables
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
  \code{\link{latex.table.fac}}
}
\examples{

}

\keyword{univar}
\keyword{IO}
\keyword{print}


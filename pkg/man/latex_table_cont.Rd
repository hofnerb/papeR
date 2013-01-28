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
                 incl_outliers = TRUE, drop = TRUE,
                 show.NAs = any(is.na(data[, variables])))

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
    logical (default: TRUE). Determines wether continuous variables,
    which contain only missing values are dropped from the table.
  }
  \item{show.NAs}{
    logical. Determines if the number of missings (NAs) is displayed as
    a separate column. Per default, \code{show.NAs} is \code{TRUE} if
    there are any missings in the variables to be displayed (and
    \code{FALSE} if not).
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
## Use dataset Orthodont
data(Orthodont, package = "nlme")

## Get summary for continuous variables
latex.table.cont(Orthodont)

## Change statistics to display
latex.table.cont(Orthodont, quantiles = FALSE)
latex.table.cont(Orthodont, count = FALSE, quantiles = FALSE)
latex.table.cont(Orthodont, mean_sd = FALSE)

## Don't drop column 'Missing' even if no missings are present
latex.table.cont(Orthodont, drop = FALSE)

## Change variables to display
latex.table.cont(Orthodont, variables = "age")

## What happens in the display if we introduce some missing values:
set.seed(1907)
Orthodont$age[sample(nrow(Orthodont), 20)] <- NA
latex.table.cont(Orthodont)

## We can also have a look at the returned object:
df.cont <- latex.table.cont(Orthodont)
df.cont
}

\keyword{univar}
\keyword{IO}
\keyword{print}


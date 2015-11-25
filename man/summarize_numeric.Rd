\name{summarize_numeric}
\alias{summarize_numeric}

\title{ Produce Summary Tables for Data Sets}
\description{
  The function produces summary tables for continuous
  variables. The obtained tables can be used directly in R, with LaTeX
  and HTML (by using the \code{\link[xtable]{xtable}} function) or Markdown
  (e.g. by using the function \code{\link[knitr]{kable}}).
}
\usage{
summarize_numeric(data,
    variables = names(data), variable.labels = labels, labels = NULL,
    group = NULL, test = !is.null(group), colnames = NULL,
    digits = 2, digits.pval = 3, smallest.pval = 0.001,
    sep = !is.null(group), sanitize = TRUE,
    drop = TRUE, show.NAs = any(is.na(data[, variables])),
    ## additional specific arguments
    count = TRUE, mean_sd = TRUE, quantiles = TRUE,
    incl_outliers = TRUE, ...)
}

\arguments{
  \item{data}{
    data set to be used.
  }
  \item{variables}{
    variables that should be included in the table.
    For details see \code{\link{summarize}}.
  }
  \item{variable.labels, labels}{
    labels for the variables.
    For details see \code{\link{summarize}}.
  }
  \item{group}{
    character specifying a grouping factor.
    For details see \code{\link{summarize}}.
  }
  \item{test}{
    logical or charachter specifying test for group differences.
    For details see \code{\link{summarize}}.
  }
  \item{colnames}{
    a vector of character strings of appropriate length.
    For details see \code{\link{summarize}}.
  }
  \item{digits}{
    number of digits to round to. Per default all values are rounded to
    two digits.
  }
  \item{digits.pval}{
    number of significant digits used for p-values.
  }
  \item{smallest.pval}{
    determines the smallest p-value to be printed exactly.
    For details see \code{\link{summarize}}.
  }
  \item{sep}{
    logical (default: \code{TRUE} if grouping specified, \code{FALSE} otherwise).
    Determines whether separators (lines) should be added after
    each variable.
  }
  \item{sanitize}{
    logical (default: \code{TRUE}) or a sanitizing function.
    For details see \code{\link{summarize}}.
  }
  \item{drop}{
    logical (default: \code{TRUE}). Determines whether variables, which contain
    only missing values are dropped from the table.
  }
  \item{show.NAs}{
    logical. Determines if the number of missings (NAs) is displayed as
    a separate column. Per default, \code{show.NAs} is \code{TRUE} if
    there are any missings in the variables to be displayed (and
    \code{FALSE} if not).
  }
  \item{count}{
    (logical) indicator if number of complete cases ("n") should be
    included in the table (default: \code{TRUE}).
  }
  \item{mean_sd}{
    (logical) indicator if mean and standard deviation should be
    included in the table (default: \code{TRUE}).
  }
  \item{quantiles}{
    (logical) indicator if quantiles (including min and max) should be
    included in the table (default: \code{TRUE}).
  }
  \item{incl_outliers}{
    Per default we use \code{\link{fivenum}} to compute the quantiles
    (if \code{quantiles = TRUE}). If extreme values should be excluded
    from min/max in the table, \code{boxplot( , plot = FALSE)$stats} is
    used instead.
  }
  \item{...}{
    additional arguments. Currently not used.
  }

}
\value{
  A special \code{data.frame} with additional class \code{summary}
  containing the computed statistics is returned from function
  \code{summarize}. Addtional attributes required for the
  \code{\link{xtable.summary}} or \code{\link{print.xtable.summary}}
  function are contained as attributes. These are extracted using the
  function \code{\link{get_option}}.
}
\author{
  Benjamin Hofner
}
\seealso{
  For details see \code{link{summarize}} and \code{link{summarize_factor}}.

  Conversion to LaTeX tables can be done using
  \code{\link{xtable.summary}} and \code{\link{print.xtable.summary}}.

  \code{\link{get_option}}
}
\examples{
if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

    ## Get summary for continuous variables
    summarize(Orthodont, type = "numeric")

    ## Change statistics to display
    summarize(Orthodont, quantiles = FALSE, type = "numeric")
    summarize(Orthodont, quantiles = FALSE, count = FALSE, type = "numeric")
    summarize(Orthodont, mean_sd = FALSE, type = "numeric")

    ## for more examples see ?summarize
}
}

\keyword{univar}
\keyword{IO}
\keyword{print}

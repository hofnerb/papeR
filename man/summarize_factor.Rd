\name{summarize_factor}
\alias{summarize_factor}

\title{ Produce Summary Tables for Data Sets}
\description{
  The function produces summary tables for factor variables.
  The obtained tables can be used directly in R, with LaTeX
  and HTML (by using the \code{\link[xtable]{xtable}} function) or Markdown
  (e.g. by using the function \code{\link[knitr]{kable}}).
}
\usage{
summarize_factor(data,
    variables = names(data), variable.labels = labels, labels = NULL,
    group = NULL, test = !is.null(group), colnames = NULL,
    digits = 3, digits.pval = 3, smallest.pval = 0.001,
    sep = TRUE, sanitize = TRUE, drop = TRUE,
    show.NAs = any(is.na(data[, variables])),
    ## additional specific arguments
    percent = TRUE, cumulative = FALSE,
    na.lab = "<Missing>", ...)
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
    number of digits to round to  (only used for fractions). Per default
    all values are rounded to three digits.
  }
  \item{digits.pval}{
    number of significant digits used for p-values.
  }
  \item{smallest.pval}{
    determines the smallest p-value to be printed exactly.
    For details see \code{\link{summarize}}.
  }
  \item{sep}{
    logical (default: \code{TRUE}). Determines whether separators
    (lines) should be added after each variable.
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
    logical. Determines if NAs are displayed as a separate category for
    each factor variable with missings. If \code{TRUE}, an additional
    statistic which includes the missings is displayed (see Examples).
    Per default, \code{show.NAs} is \code{TRUE} if there are any
    missings in the variables to be displayed (and \code{FALSE} if not).
  }
  \item{percent}{
    logical. Should the fractions be given as percent values? Otherwise,
    fractions are given as decimal numbers.
  }
  \item{cumulative}{
    logical. Should cumulative fractions be displayed?
  }
  \item{na.lab}{
    label for missing values (default: \code{"<Missing>"}).
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
## Example requires package nlme to be installed and loaded
if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

    ## Get summary for continuous variables
    summarize(Orthodont, type = "factor")

    ## Reorder data for table:
    summarize(Orthodont, variables = c("Sex", "Subject"), type = "factor")

    ## What happens in the display if we introduce some missing values:
    set.seed(1907)
    Orthodont$Sex[sample(nrow(Orthodont), 20)] <- NA
    summarize(Orthodont, type = "factor")
    summarize(Orthodont, variables = "Sex", type = "factor")
    ## do not show statistics on missing values
    summarize(Orthodont, variables = "Sex", show.NAs = FALSE, type = "factor")
}
}

\keyword{univar}
\keyword{IO}
\keyword{print}

\name{summarize}
\alias{summarize}
\alias{summarise}

\title{ Produce Summary Tables for Data Sets}
\description{
  The function produces summary tables for factors and continuous
  variables. The obtained tables can be used directly in R, with LaTeX
  and HTML (by using the \code{\link[xtable]{xtable}} function) or Markdown
  (e.g. by using the function \code{\link[knitr]{kable}}).
}
\usage{
summarize(data, type = c("numeric", "factor"),
    variables = names(data), variable.labels = labels, labels = NULL,
    group = NULL, test = !is.null(group), colnames = NULL,
    digits = NULL, digits.pval = 3, smallest.pval = 0.001,
    sep = NULL, sanitize = TRUE, drop = TRUE,
    show.NAs = any(is.na(data[, variables])), ...)
}

\arguments{
  \item{data}{
    data set to be used.
  }
  \item{type}{
    print summary table for either \code{numeric} or \code{factor}
    variables.
  }
  \item{variables}{
    character vector defining variables that should be included in the
    table. Per default, all numeric or factor variables of \code{data}
    are used, depending on \code{type}.
  }
  \item{variable.labels, labels}{
    labels for the variables. If \code{variable.labels = NULL} (default)
    \code{variables} is used as label. If \code{variable.labels = TRUE},
    \code{labels(data, which = variables)} is used as labels. Instead of
    \code{variable.labels} one can also use \code{labels}.
  }
  \item{group}{
    character specifying a grouping factor. Per default no grouping is
    applied.
  }
  \item{test}{
    logical or character string. If a \code{group} is given, this argument
    determines whether a test for group differences is computed. For
    details see \code{\link{summarize_numeric}} and
    \code{\link{summarize_factor}}.
  }
  \item{colnames}{
    a vector of character strings of appropriate length.
    The vector supplies alternative column names for the resulting
    table. If \code{NULL} default names are used.
  }
  \item{digits}{
    number of digits to round to. For defaults see
    \code{\link{summarize_numeric}} and \code{\link{summarize_factor}}.
  }
  \item{digits.pval}{
    number of significant digits used for p-values.
  }
  \item{smallest.pval}{
    determines the smallest p-value to be printed
    exactly. Smaller values are given as \dQuote{< smallest.pval}.
    This argument is passed to the \code{eps} argument of
    \code{\link{format.pval}}. See there for details.
  }
  \item{sep}{
    logical. Determines whether separators (lines) should be added after
    each variable. For defaults see \code{\link{summarize_numeric}} and
    \code{\link{summarize_factor}}.
  }
  \item{sanitize}{
    logical (default: \code{TRUE}) or a sanitizing function used to clean the
    input in order to be useable in LaTeX environments. Per default
    \code{\link{toLatex.character}} is used.
  }
  \item{drop}{
    logical (default: \code{TRUE}). Determines whether variables, which contain
    only missing values are dropped from the table.
  }
  \item{show.NAs}{
    logical. Determines if NAs are displayed. Per default,
    \code{show.NAs} is \code{TRUE} if there are any missings in the
    variables to be displayed (and \code{FALSE} if not). For details see
    \code{\link{summarize_numeric}} and \code{\link{summarize_factor}}.
  }
  \item{...}{
    additional arguments for \code{\link{summarize_numeric}} and
    \code{\link{summarize_factor}}. See there for details.
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
  For details see
  \code{\link{summarize_numeric}} and \code{\link{summarize_factor}}.

  Conversion to LaTeX tables can be done using
  \code{\link{xtable.summary}} and \code{\link{print.xtable.summary}}.

  \code{\link{get_option}}
}
\examples{
if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

    ## Get summary for continuous variables
    (tab1 <- summarize(Orthodont, type = "numeric"))

    ## Change statistics to display
    summarize(Orthodont, quantiles = FALSE, type = "numeric")
    summarize(Orthodont, quantiles = FALSE, count = FALSE, type = "numeric")
    summarize(Orthodont, mean_sd = FALSE, type = "numeric")

    ## Get summary for categorical variables
    (tab2 <- summarize(Orthodont, type = "fac"))

    ## use fraction instead of percentage
    summarize(Orthodont, percent = FALSE, type = "fac")

    ## Using the tables with Markdown
    if (require("knitr")) {
        kable(tab1)
        kable(tab2)
    }

    ## Using the tables with LaTeX
    if (require("xtable")) {
        xtable(tab1)
        ## grouped table
        xtable(summarize(Orthodont, group = "Sex"))
        xtable(tab2)
    }
}
}

\keyword{univar}
\keyword{IO}
\keyword{print}

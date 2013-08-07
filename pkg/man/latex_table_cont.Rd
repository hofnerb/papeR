\name{latex.table.cont}
\alias{latex.table.cont}
\alias{print.table.cont}

\title{ Produce (LaTeX) Summaries for Continuous Variables }
\description{
  The function produces LaTeX tables with summary statistics for continous
  variables. It makes use of the booktabs package in LaTeX to obtain
  tables with a nice layout.
}
\usage{

latex.table.cont(data, variables = names(data), labels = NULL, group = NULL,
                 colnames = NULL, digits = 2, table = c("tabular", "longtable"),
                 align = NULL, caption = NULL, label = NULL, floating = FALSE,
                 center = TRUE, sep = !is.null(group), sanitize = TRUE,
                 count = TRUE, mean_sd = TRUE, quantiles = TRUE,
                 incl_outliers = TRUE, drop = TRUE,
                 show.NAs = any(is.na(data[, variables])))

\method{print}{table.cont}(x,
      colnames = get_options(x, "colnames"),
      table = get_options(x, "table"),
      align = get_options(x, "align"),
      caption = get_options(x, "caption"),
      label = get_options(x, "label"),
      floating = get_options(x, "floating"),
      center = get_options(x, "center"),
      sep = get_options(x, "sep"),
      sanitize = get_options(x, "sanitize"),
      count = get_options(x, "count"),
      mean_sd = get_options(x, "mean_sd"),
      quantiles = get_options(x, "quantiles"),
      ...)

}

\arguments{
  \item{data}{
    data set to be used
  }
  \item{variables}{
    character vector defining (continuous) variables that should be
    included in the table. Per default, all non-factor variables of
    \code{data} are used.
  }
  \item{labels}{
    labels for the variables. If \code{labels = NULL} (default)
    \code{variables} is used as label. If \code{labels = TRUE},
    \code{labels(data, which = variables)} is used as
    labels.
  }
  \item{group}{
    character specifying a grouping factor. Per default no grouping is
    applied.
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
  \item{caption}{
    (optional) character string. Caption of LaTeX table. Note that
    captions are suported for all tables (see also details below).
  }
  \item{label}{
    (optional) character string. Label of LaTeX table specified as
    \code{\\label{"label"}}.
  }
  \item{floating}{
    logical (default: FALSE). Determines wether the table is a floating
    object (i.e. use a \code{table} environment or not). Note that a
    \code{longtable} cannot be a floating object but captions can be
    used.
  }
  \item{center}{
    logical (default: TRUE). Determines if table should be centered.
  }
  \item{sep}{
    logical (default: TRUE if grouping specified, FALSE otherwise).
    Determines wether separators (lines) should be added after each variable.
  }
  \item{sanitize}{
    logical (default: TRUE) or a sanitizing function used to clean the
    input in order to be useable in LaTeX environments. Per default
    \code{\link{toLatex.character}} is used.
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
  \item{x}{
    An object of class \code{table.fac}. See Value for details.
  }
  \item{...}{
    additional arguments for printing. Currently not used.
  }
}
\details{
  The output requires \code{\\usepackage{booktabs}} in the LaTeX file.

  Captions can be added to both, \code{longtable}s and \code{tabular}s.
  In the latter case, captions are also suported if the table is no
  floating object. In this case, the LaTeX package \code{capt-of} is
  required.
}
\value{
  A special \code{data.frame} with additional class \code{table.cont}
  containing the computed statistics is returned from function
  \code{latex.table.cont}. Addtional attributes required for the print
  function are contained (see \code{attr(x, "latex.table.options")}).
  These are extracted in the print function using the function
  \code{\link{get_options}}.

  The output is printed with LaTeX style syntax highlighting to be used
  e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{latex.table.fac}} and \code{\link{get_options}}
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

## Show column 'Missing' even if no missings are present
latex.table.cont(Orthodont, show.NAs = TRUE)

## Change variables to display
latex.table.cont(Orthodont, variables = "age")

## What happens in the display if we introduce some missing values:
set.seed(1907)
Orthodont$age[sample(nrow(Orthodont), 20)] <- NA
latex.table.cont(Orthodont)

## We can also have a look at the returned object:
df.cont <- latex.table.cont(Orthodont)
## Structure of object
str(df.cont)
## Make object a data.frame and have a look at the raw data
class(df.cont) <- "data.frame"
df.cont

print.table.cont(df.cont)
}

\keyword{univar}
\keyword{IO}
\keyword{print}


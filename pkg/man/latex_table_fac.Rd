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

latex.table.fac(data, variables = names(data), labels = NULL, group = NULL,
                colnames = NULL, digits = 2, table = c("tabular", "longtable"),
                align = NULL, caption = NULL, label = NULL, floating = FALSE,
                center = TRUE, sep = TRUE, sanitize = TRUE, drop = TRUE,
                show.NAs = any(is.na(data[, variables])), na.lab = "<Missing>")

\method{print}{table.fac}(x,
      colnames = get_options(x, "colnames"),
      table = get_options(x, "table"),
      align = get_options(x, "align"),
      caption = get_options(x, "caption"),
      label = get_options(x, "label"),
      floating = get_options(x, "floating"),
      center = get_options(x, "center"),
      sep = get_options(x, "sep"),
      sanitize = get_options(x, "sanitize"),
      ...)

}

\arguments{
  \item{data}{
    data set to be used
  }
  \item{variables}{
    character vector defining (factor) variables that should be included
    in the table. Per default, all factor variables of \code{data} are
    used.
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
    logical (default: TRUE). Determines wether separators (lines) should
    be added after each factor variable.
  }
  \item{sanitize}{
    logical (default: TRUE) or a sanitizing function used to clean the
    input in order to be useable in LaTeX environments. Per default
    \code{\link{toLatex.character}} is used.
  }
  \item{drop}{
    logical (default: TRUE). Determines wether factor variables,
    which contain only missing values are dropped from the table.
  }
  \item{show.NAs}{
    logical. Determines if NAs are displayed as a separate category for
    each factor variable with missings. If \code{TRUE}, an additional
    statistic which includes the missings is displayed (see Examples).
    Per default, \code{show.NAs} is \code{TRUE} if there are any
    missings in the variables to be displayed (and \code{FALSE} if not).
  }
  \item{na.lab}{
    label for missing values (default: \code{"<Missing>"}).
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
  A special \code{data.frame} with additional class \code{table.fac}
  containing the computed statistics is returned from function
  \code{latex.table.cont}. Addtional attributes required for the print
  function are contained (see \code{attr(x, "latex.table.options")}).

  The output is printed with LaTeX style syntax highlighting to be used
  e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{latex.table.cont}}
}
\examples{
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

\keyword{univar}
\keyword{IO}
\keyword{print}

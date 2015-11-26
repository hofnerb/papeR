\name{xtable.summary}
\alias{xtable}
\alias{xtable.summary}
\alias{print.xtable}
\alias{print.xtable.summary}

\title{ Create And Print Tables With Markup }
\description{
  The function produces objects which can be printed to LaTeX
  and HTML code.
}
\usage{

\method{xtable}{summary}(x, caption = NULL, label = NULL, align = NULL,
       digits = NULL, display = NULL, ...)

\method{print}{xtable.summary}(x, rules = NULL, header = NULL,
      caption.placement = getOption("xtable.caption.placement", "top"),
      hline.after = getOption("xtable.hline.after", NULL),
      add.to.row = getOption("xtable.add.to.row", NULL),
      include.rownames = getOption("xtable.include.rownames", FALSE),
      booktabs = getOption("xtable.booktabs", TRUE),
      sanitize.text.function = get_option(x, "sanitize"),
      tabular.environment = getOption("xtable.tabular.environment", "tabular"),
      floating = getOption("xtable.floating", FALSE),
      ...)

}

\arguments{
  \item{x}{
    object of class \code{"summary"}, which is produced by the
    function \code{\link{summarize}} or an object of class
    \code{"xtable.summary"} produced by \code{xtable}.
  }
  \item{caption}{
    character vector specifying the table's caption; see
    \code{\link[xtable]{xtable}} for details.
  }
  \item{label}{
    character string specifying the LaTeX label or HTML anchor; see
    \code{\link[xtable]{xtable}} for details.
  }
  \item{align}{
    character string specifying the alignment of table columns; see
    \code{\link[xtable]{xtable}} for details.
  }
  \item{digits}{
    numeric vector specifying the number of digits to display in each
    column; see \code{\link[xtable]{xtable}} for details.
  }
  \item{display}{
    character string specifying the column types; see
    \code{\link[xtable]{xtable}} for details.
  }

  \item{rules}{
    character string specifying the rules to be used. Per default the
    \code{rules} are defined by \code{\link{summarize}} and subsequently
    extracted from \code{x} via \code{get_option(x, "rules")}.
  }
  \item{header}{
    character string specifying the table header to be used. Per default the
    \code{header} is defined by \code{\link{summarize}} and subsequently
    extracted from \code{x} via \code{get_option(x, "header")}.
  }
  \item{caption.placement}{
    can be either \code{"bottom"} or \code{"top"} (default). Note that
    the standard default of \code{\link[xtable]{print.xtable}}
    is \code{"bottom"}.
  }
  \item{hline.after}{
    vector indicating the rows after which a horizontal line is printed.
    Here, the default is to not draw hlines (i.e. \code{hline.after =
    NULL}) and horizontal lines are added via \code{add.to.row}
    (see there for details). Note that the standard default of
    \code{\link[xtable]{print.xtable}} is \code{c(-1,0,nrow(x))}.
  }
  \item{add.to.row}{
    list of row numbers (\code{pos}) and text (\code{command}) to be
    added to the specified rows. Per default, top and bottom rules are
    added to the table and a rule specified in \code{rules} is added
    below the heading. If \code{sep = TRUE} in \code{\link{summarize}}
    additional separators (as specified in \code{rules}) are added after
    each variable.
  }
  \item{include.rownames}{
    logical. If \code{TRUE} the rows names are printed.
    Default value is \code{FALSE}. Note that the standard default of
    \code{\link[xtable]{print.xtable}} is \code{TRUE}.
  }
  \item{booktabs}{
    logical. If \code{TRUE} (default), the \code{toprule},
    \code{midrule} and \code{bottomrule} tags from the LaTeX package
    \code{"booktabs"} are used rather than \code{hline} for the
    horizontal line tags. Note that the standard default of
    \code{\link[xtable]{print.xtable}} is \code{FALSE}.
  }
  \item{sanitize.text.function}{
    All non-numeric enteries (except row and column names) are sanitised
    in an attempt to remove characters which have special meaning for
    the output format. Per default the function \code{\link{toLatex}} is
    used to sanitize the text. For more options see
    \code{\link[xtable]{print.xtable}}.
  }
  \item{tabular.environment}{
    character string. Per default \code{"tabular"} is used. For long
    tables that span over more than one page, one can use
    \code{"longtable"}. For more options see
    \code{\link[xtable]{print.xtable}}.
  }
  \item{floating}{
    logical. Determine if the table is printed in a floating
    environment. Note that the standard default of
    \code{\link[xtable]{print.xtable}} is \code{TRUE}. See there for
    details.
  }
  \item{...}{
    additional arguments passed to \code{\link[xtable]{xtable}} or
    \code{\link[xtable]{print.xtable}}. See there for details.
  }
}
\details{
  We use the standard \code{xtable} function but add a special class
  that allows different defaults in the \code{print.xtable} function.

  In general, all options of \code{print.xtable} can be used as well as
  global options set via \code{options()}. E.g.
  \code{options(xtable.booktabs = FALSE}
  will set the argument \code{booktabs} per default to
  \code{FALSE} for all calls to \code{print.xtable}.
}
\value{
  After printing, a table with LaTeX markup is returned.
}
\author{
  Benjamin Hofner
}
\seealso{
  For details see \code{\link[xtable]{xtable}} and
  \code{\link[xtable]{print.xtable}}.

  \code{link{summarize}}, \code{\link{get_option}}
}
\examples{
if (require("nlme")) {
    ## Use dataset Orthodont
    data(Orthodont, package = "nlme")

    ## Get summary for continuous variables
    (tab1 <- summarize(Orthodont, type = "numeric"))

    ## Get summary for categorical variables
    (tab2 <- summarize(Orthodont, type = "fac"))

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

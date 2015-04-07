\name{summarize}
\alias{summarize}
\alias{summarise}

\title{ Produce Summary Tables for Data Sets}
\description{
  The function produces summary tables for factors and continuous
  variables. The obtained tables can be used directly in R, with LaTeX
  and HTML (by using the \code{\link{xtable}} function) or Markdown
  (e.g. by using the function \code{\link{kable}}).
}
\usage{

summarize(data, type = c("numeric", "factor"),
          variables = names(data), variable.labels = NULL,
          group = NULL, test = TRUE, ...)

}

\arguments{
  \item{data}{
    data set to be used
  }
  \item{type}{
    print summary table for either \code{numeric} or \code{factor}
    variables.
  }
  \item{variables}{
    character vector defining (factor) variables that should be included
    in the table. Per default, all factor variables of \code{data} are
    used.
  }
  \item{variable.labels}{
    labels for the variables. If \code{variable.labels = NULL} (default)
    \code{variables} is used as label. If \code{variable.labels = TRUE},
    \code{labels(data, which = variables)} is used as labels.
  }
  \item{group}{
    character specifying a grouping factor. Per default no grouping is
    applied.
  }
  \item{test}{
    logical or character string. If a \code{group} is given, this argument
    determines whether a test for group differences is computed. For
    details see \code{link{summarize_numeric}} and
    \code{link{summarize_factor}}.
  }
  \item{...}{
    additional arguments for \code{link{summarize_numeric}} and
    \code{link{summarize_factor}}. See there for details.
  }

}
\details{
}
\value{
  A special \code{data.frame} with additional class \code{summary}
  containing the computed statistics is returned from function
  \code{summarize}. Addtional attributes required for the print
  function are contained as attributes. These are extracted in the print
  functions using the function \code{\link{get_option}}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{get_option}}
}
\examples{
}

\keyword{univar}
\keyword{IO}
\keyword{print}

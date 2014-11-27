\name{get_options}
\alias{get_options}

\title{ Extract printing options from \code{table.cont} and
  \code{table.fac} objects }
\description{
  Helper function to extract printing options from \code{table.cont} and
  \code{table.fac} objects as produced by \code{\link{latex.table.cont}} and
  \code{\link{latex.table.fac}}.
}
\usage{

get_options(object, name)

}

\arguments{
  \item{object}{
    \code{table.cont} or \code{table.fac} object as produced by
    \code{\link{latex.table.cont}} and \code{\link{latex.table.fac}}
  }
  \item{name}{
    name of the option, e.g. \code{"table"} and \code{"align"}. See
    \code{\link{latex.table.cont}} and \code{\link{latex.table.fac}} for
    available options.
  }
}
\value{
  Option.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{latex.table.cont}} and \code{\link{latex.table.fac}}
}


\keyword{IO}
\keyword{print}


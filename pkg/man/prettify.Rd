\name{prettify}
\alias{prettify}
\alias{prettify.data.frame}

\title{ Make Pretty Summary Tables}
\description{
  Replace variable names with labels and care for a better separation of
  variable and value label of factor variables.
}
\usage{
\method{prettify}{data.frame}(object, labels, sep = ": ", extra.column = FALSE, ...)
}
\arguments{
  \item{object}{
    object of class \code{data.frame} resulting (most likely) from a
    call to \code{\link{summary}}.
  }
  \item{labels}{
    specify labels here. For the format see \code{\link{labels}}.
  }
  \item{sep}{
    separator between variable label and value label of a factor
    variable (default: \code{": "}).
  }
  \item{extra.column}{
    logical: provide an extra column for the value labels of factors
    (default: \code{FALSE}).
  }
  \item{\dots}{
    further options. Currently not applicable.
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~

}
\value{
  \code{data.frame} with prettier variable labels.
}
\author{
  Benjamin Hofner
}

\seealso{
}
\examples{

}

\keyword{methods}
\keyword{models}

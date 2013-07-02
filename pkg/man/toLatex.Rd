\name{toLatex}
\alias{toLatex.character}

\title{ Cleaning R Code for printing in LaTeX environments }
\description{
  The function produces code that LaTeX is able to typeset.
}
\usage{
\method{toLatex}{character}(object, ...)
}

\arguments{
  \item{object}{
    an object of class character which should be cleaned for printing in
    LaTeX environments.
  }
  \item{...}{
    additional arguments. Currently not used.
  }
}
\value{
  A character string with special markup is returned: The output is
  printed with LaTeX style syntax highlighting to be used
  e.g. in Sweave chunks with \code{results=tex}.
}
\author{
  Benjamin Hofner
}
\seealso{
  \code{\link{toLatex}}
}
\examples{
txt <- "Price: <= 500$ & additional goodies"
toLatex(txt)
}

\keyword{univar}
\keyword{IO}
\keyword{print}

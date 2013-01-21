\name{summary.fixef}
\alias{summary.fixef}
\alias{summary.fixef.lme}
\alias{summary.fixef.mer}

\title{ Extract Summary Statistics for Fixed Effects from Mixed Models}
\description{
  Extract summary statistics for fixed effects from mixed models
  estimated with either \pkg{nlme} or \pkg{lme4}.
}
\usage{
\method{summary.fixef}{lme}(object, digits = NULL, ...)

\method{summary.fixef}{mer}(object, digits = NULL, ...)
}
\arguments{
  \item{object}{
    object of class \code{lme} (package \pkg{nlme}) or \code{mer}
    (package \pkg{lme4})
  }
  \item{digits}{
    number of significant digits. The default, \code{NULL}, uses
    \code{getOption(digits)}. See also argument \code{digits} in
    \code{\link{format}}.
  }
  \item{\dots}{
    further options. Currently not applicable.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{data.frame} with \code{summary} of fixed effects extracted from
  the mixed model. This \code{data.frame} can then for example be used
  with package \pkg{xtable} to produce nice LaTeX outputs.
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

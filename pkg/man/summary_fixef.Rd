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
\method{summary.fixef}{lme}(object, digits = NULL, scientific = FALSE,
              smallest.pval = 0.001, ci = TRUE, ...)

\method{summary.fixef}{mer}(object, digits = NULL, scientific = FALSE,
              smallest.pval = 0.001, ci = TRUE, ...)
}
\arguments{
  \item{object}{
    object of class \code{lme} (package \pkg{nlme}) or \code{mer}
    (package \pkg{lme4})
  }
  \item{digits}{
    number of significant digits. The default, \code{NULL}, uses
    \code{getOption("digits")}. See also argument \code{digits} in
    \code{\link{format}}.
  }
  \item{scientific}{specifies if numbers should be printed in scientific
    format. For details and possible values see \code{\link{format}}.
  }
  \item{smallest.pval}{determines the smallest p-value to be printed
    exactly. Smaller values are given as \dQuote{< smallest.pval}.
    This argument is passed to the \code{eps} argument of
    \code{\link{format.pval}}. See there for details.
  }
  \item{ci}{logical. Should confidence intervals be added?}
  \item{\dots}{
    further options passed to \code{\link{format}} and
    \code{\link{format.pval}}.
  }
}
\details{
  All values (except p-values) are rounded using \code{\link{zapsmall}}
  with default value for \code{digits} (which can be altered by the
  global \code{"digits"} option) and then passed to \code{\link{format}}
  for pretty printing.

  P-values are formated using \code{\link{format.pval}}. Note that the
  \code{eps} argument of \code{\link{format.pval}} is provided by
  \code{smallest.pval}. Note that the choice of \code{smallest.pval}
  also affects the number of digits for p-values as these are rounded to
  the same number of digits as given in \code{smallest.pval}.
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
## Load data set Orthodont
data(Orthodont, package = "nlme")

## With package nlme:
require("nlme")
## Fit a model for distance with random intercept for Subject
mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1 | Subject)
summary(mod)
## Extract fixed effects table only
summary.fixef(mod)

## With package lme4:
require("lme4")
## Fit a model for distance with random intercept for Subject
mod4 <- lmer(distance ~ age + Sex + (1|Subject), data = Orthodont)
summary(mod4)
## Extract fixed effects table only
summary.fixef(mod4)

## change display
summary.fixef(mod, digits = 2)
summary.fixef(mod, scientific = -1)  # set penalty for scientific
summary.fixef(mod, digits = 2, smallest.pval = 0.0001)
}

\keyword{methods}
\keyword{models}

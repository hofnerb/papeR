\name{confint.lme}
\alias{confint.lme}
\alias{confint.mer}
\title{Confidence intervals for mixed models}
\usage{
\method{confint}{lme}(object, parm, level = 0.95, ...)
\method{confint}{mer}(object, parm, level = 0.95,
            simulate = c("ifneeded", TRUE, FALSE),
            B = 1000,...)
}
\arguments{
  \item{object}{
    Model of class \code{lme} or \code{mer}.
  }

  \item{parm}{
    Parameters to be included in the confidence interval. See
    \code{\link{confint.default}} for details.
  }

  \item{level}{
    the confidence level.
  }

  \item{simulate}{
    If \dQuote{"ifneeded"} is specified (default), simulated confidence
    intervals are returned if (and only if) no z-value exists in the
    corresponding \code{summary} and asymptotic confidence intervals
    will be returned elsewhise. If \code{TRUE} (or \dQuote{"TRUE"})
    confidence intervals will be estimated using \code{\link{ci}} from
    package \pkg{gmodels} which uses \code{\link{mcmcsamp}} internally.
    If \code{FALSE} (or \dQuote{"FALSE"}), asymptotic confidence
    intervals will be returned and an error is given if not possible.
  }

  \item{B}{
    number of samples to take in \code{\link{mcmcsamp}}. Per default
    1000 samples are used.
  }

  \item{...}{
    Additional arguments. Currently not used.
  }
}
\value{
  Matrix with confidence intervalls.
}
\description{
  Compute confidence intervals for mixed models from
  packages \pkg{nlme} and \pkg{lme4}
}
\author{
  Benjamin Hofner, partially based on code from package stats.
  See source code for documentation.
}


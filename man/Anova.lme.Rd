\name{Anova.lme}
\alias{Anova.lme}

\title{ Anova Function for lme Models }
\description{
  This is a wrapper to \code{\link{anova.lme}} from package \pkg{nlme}
  and is coded similar to \code{\link[car]{Anova}} from \pkg{car} as it
  produces marginal tests by default.
}
\usage{
\method{Anova}{lme}(mod, type = c("marginal", "sequential"), ...)
}

\arguments{
  \item{mod}{
    linear mixed model fitted with package \pkg{nlme}.
  }
  \item{type}{
    type of anova, either marginal (default) or sequential.
  }
  \item{\dots}{
    further arguments to be passed to \code{\link{anova.lme}}
  }
}

\seealso{
  \code{\link[car]{Anova}} (package \pkg{car})
}
\examples{
## Example requires package nlme to be installed and loaded
if (require("nlme")) {
    ## Load data set Orthodont
    data(Orthodont, package = "nlme")

    ## Fit a model for distance with random intercept for Subject
    mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1 | Subject)

    Anova(mod)
}
}

\keyword{methods}
\keyword{models}

\name{prettify}
\alias{prettify}
\alias{prettify.anova}
\alias{prettify.data.frame}
\alias{prettify.summary.lm}
\alias{prettify.summary.glm}
\alias{prettify.summary.coxph}
\alias{prettify.summary.lme}
\alias{prettify.summary.mer}
\alias{prettifyPValue}

\title{Make Pretty Summary Tables}
\description{
  Replace variable names with labels and care for a better separation of
  variable and value label of factor variables. Additionally, confidence
  intervalls can be easily added and p-values are formated for pretty printing.
}
\usage{
\method{prettify}{data.frame}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                    smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                    signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.lm}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                    confint = TRUE, level = 0.95,
                    smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                    signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.glm}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                     confint = TRUE, level = 0.95, OR = TRUE,
                     smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                     signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.coxph}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                       confint = TRUE, level = 0.95, HR = TRUE,
                       smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                       signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.lme}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                    confint = TRUE, level = 0.95,
                    smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                    signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.mer}(object, labels = NULL, sep = ": ", extra.column = FALSE,
                    confint = TRUE, level = 0.95,
                    smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                    signif.stars = getOption("show.signif.stars"),
                    simulate = c("ifneeded", TRUE, FALSE), B = 1000, ...)

\method{prettify}{anova}(object, labels,
               smallest.pval = 0.001, digits = NULL, scientific = FALSE,
               signif.stars = getOption("show.signif.stars"), ...)

## helper function for pretty p-values
prettifyPValue(object, smallest.pval = 0.001, digits = NULL, scientific = FALSE,
               signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{
    object of class \code{data.frame} resulting (most likely) from a
    call to \code{\link{summary}} or directly the output from
    \code{\link{summary}}, \code{\link{anova}} or \code{\link{Anova}}
    (the latter from package \pkg{car}).
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
  \item{confint}{
    logical. Should confidence intervals be added?
  }
  \item{level}{
    confidence level; Per default 0.95\% confidence
    intervals are returned
  }
  \item{OR}{
    logical. Should odds ratios be added? Only applicable if a logistic
    regression model was fitted (i.e., with \code{family = "binomial"}).
  }
  \item{HR}{
    logical. Should hazard ratios be added?
  }
  \item{smallest.pval}{
    determines the smallest p-value to be printed
    exactly. Smaller values are given as \dQuote{< smallest.pval}.
    This argument is passed to the \code{eps} argument of
    \code{\link{format.pval}}. See there for details.
  }
  \item{digits}{
    number of significant digits. The default, \code{NULL}, uses
    \code{getOption("digits")} for formating p-values and leaves all
    other columns unchanged. If \code{digits} are specified, all columns
    use this number of significant digits (columnwise). See also
    argument \code{digits} in \code{\link{format}}.
  }
  \item{scientific}{specifies if numbers should be printed in scientific
    format. For details and possible values see \code{\link{format}}.
  }
  \item{signif.stars}{
    logical (default = TRUE). Should significance stars be added? Per
    default system options are used. See \code{ getOption("show.signif.stars")}.
  }
  \item{simulate}{
    should the asysmptotic or simulated confidence intervals be used?
    See \code{\link{confint.mer}} for details.
  }
  \item{B}{
    number of samples to take in \code{\link{mcmcsamp}}. See
    \code{\link{confint.mer}} for details.
  }
  \item{\dots}{
    further options. Currently not applicable.
  }
}
\details{
  Specialized functions that prettify summary tables of various models
  exist. For the \code{data.frame} method, \code{extra.column} and
  \code{sep} can only be used if \code{labels} are specified as variable
  names need to be known in order to split variable name and factor
  level. For \code{\link{summary}} objects, variable names can be extracted
  from the objects.

  To compute confidence intervalls, the model is refitted internally
  extrating the call and environment from the model summary. All
  functions then use \code{\link{confint}} on the refitted model. For
  \code{lme} and \code{mer} models special
  \code{\link[=confint.mer]{confint}} functions are defined in this
  package. For details see there.

  \code{prettifyPValue} is a helper function used within the prettify
  functions but can also be used directly on a \code{data.frame} object.
  The function tries to \dQuote{guess} the column of p-values and
  formats them nicely. Additionally, significance stars are added if requested.

}
\value{
  \code{data.frame} with prettier variable labels.
  For summary functions additionally confidence
  intervalls (if requested), odds ratio (for logistic regression models,
  if requested), p-values formated for pretty printing and significance
  stars (if requested) are attached.
}
\author{
  Benjamin Hofner
}

\seealso{
  \code{\link{summary}}, \code{\link{summary.lm}},
  \code{\link{summary.glm}}, \code{\link{summary.lme}},
  \code{\link[=summary.mer-class]{summary.mer}} and \code{\link{summary.coxph}}
  for summary functions.

  \code{\link{anova}} and \code{\link{Anova}} for ANOVA functions.

  \code{\link{confint}} and \code{\link{ci}} for confidence intervals.
  Special functions are implemented for mixed models:
  \code{\link{confint.lme}} and \code{\link{confint.mer}}.
}
\examples{
## Load data set Orthodont
data(Orthodont, package = "nlme")

## Fit a linear model
linmod <- lm(distance ~ age + Sex, data = Orthodont)
## Extract pretty summary
prettify(summary(linmod))

### (fit a more suitable model with random effects)
## With package nlme:
require("nlme")
## Fit a model for distance with random intercept for Subject
mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1 | Subject)
summary(mod)
## Extract fixed effects table, add confidence interval and make it pretty
prettify(summary(mod))
## Extract fixed effects table only and make it pretty
prettify(summary(mod), confint = FALSE)

## With package lme4:
require("lme4")
## Fit a model for distance with random intercept for Subject
mod4 <- lmer(distance ~ age + Sex + (1|Subject), data = Orthodont)
summary(mod4)
## Extract fixed effects table and make it pretty
prettify(summary(mod4))

## Extract anova (sequential tests)
anova(linmod)
## now prettify it
prettify(anova(linmod))

## Extract Anova (partial instead of sequential tests)
library("car")
Anova(mod4)
## now prettify it
prettify(Anova(mod4))
}

\keyword{methods}
\keyword{models}

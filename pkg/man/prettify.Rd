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

\title{Make Pretty Summary and Anova Tables}
\description{
  Improve summary tables by replacing variable names with labels and
  separating variable names and value labels of factor variables.
  Additionally, confidence intervalls are added to summaries per default and
  p-values are formated for pretty printing.
}
\usage{
## generic function called by all prettify.summary.xxx functions
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
         signif.stars = getOption("show.signif.stars"),
         env = parent.frame(), ...)

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
prettifyPValue(object, smallest.pval = 0.001, digits = NULL,
               scientific = FALSE,
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
    logical value indicating if confidence intervals sould be added or
    the confidence intervals themself.

    Using \code{confint = TRUE} is experimental only and special care
    needs to be taken that the data set used for fitting is neither
    changed nor deleted. See \sQuote{Details} and \sQuote{Examples}.
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
  \item{env}{
    specify environment in which the model was fitted. Needed to find
    the correct data for refitting the model in order to obtain
    confidence intervals.
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
  package. For details see there. Note that is it highly important
  \bold{not} to modify or delete the data in the fitting environment if
  one wants to obtain correct confidence intervals. See examples for what
  might happen. We try ourt best to find changes of the data and to warn
  the user (but without any warranty).

  Alternatively, one can directly specify the confidence intervals using
  e.g. \code{confint = confint(model)}, where \code{model} is the fitted
  model. This does not rely on refitting of the model and should always
  work as expected. In this case, arguments \code{level},
  \code{simulate} and \code{B} are ignored. Note that in this case it is
  adviced to also specify the labels by hand!

  \code{prettifyPValue} is a helper function used within the prettify
  functions but can also be used directly on a \code{data.frame} object.
  The function tries to (cleverly) \dQuote{guess} the column of p-values
  (based on the column names) and formats them nicely. Additionally,
  significance stars are added if requested.

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

######################################################################
# Linear model
######################################################################

## Fit a linear model
linmod <- lm(distance ~ age + Sex, data = Orthodont)
## Extract pretty summary
prettify(summary(linmod))

## Extract anova (sequential tests)
anova(linmod)
## now prettify it
prettify(anova(linmod))

######################################################################
# Random effects model (nlme)
######################################################################

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

######################################################################
# Random effects model (lme4)
######################################################################

## With package lme4:
require("lme4")
## Fit a model for distance with random intercept for Subject
mod4 <- lmer(distance ~ age + Sex + (1|Subject), data = Orthodont)
summary(mod4)
## Extract fixed effects table and make it pretty
prettify(summary(mod4))

## Extract and prettify anova (sequential tests)
prettify(anova(mod4))

## Better: extract Anova (partial instead of sequential tests)
library("car")
try(Anova(mod4))  ## doesn't currently work with lme4 >= vers. 1.0.0
## now prettify it
try(prettify(Anova(mod4))) ## doesn't currently work with lme4 >= vers. 1.0.0

######################################################################
# Cox model
######################################################################

library("survival")
## Load data set ovarian
data(ovarian, package = "survival")

## fit a Cox model
mod5 <- coxph(Surv(futime, fustat) ~ age, data=ovarian)
summary(mod5)
## Make pretty summary
prettify(summary(mod5))

## Make pretty summary
prettify(Anova(mod5))



######################################################################
# ATTENTION when confint = TRUE: Do not modify or delete data
######################################################################

## Fit a linear model (same as above)
linmod <- lm(distance ~ age + Sex, data = Orthodont)
## Extract pretty summary
prettify(summary(linmod))

## Change the data (age in month instead of years)
Orthodont$age <- Orthodont$age * 12
prettify(summary(linmod))  ## confidence intervals for age have changed
                           ## but coefficients stayed the same; a
                           ## warning is issued

## Remove data in fitting environment
rm(Orthodont)
prettify(summary(linmod))  ## confidence intervals are missing as no
                           ## data set was available to refit the model



######################################################################
# Use confint to specify confidence interval without refitting
######################################################################

## make labels without using the data set
labels <- c("distance", "age", "Subject", "Sex")
names(labels) <- labels

## usually easier via: labels(Orthodont)

prettify(summary(linmod), confint = confint(linmod),
         labels = labels)

}

\keyword{methods}
\keyword{models}

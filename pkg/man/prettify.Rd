\name{prettify}
\alias{prettify}
\alias{prettify.data.frame}
\alias{prettify.summary.lm}
\alias{prettify.summary.glm}

\title{ Make Pretty Summary Tables}
\description{
  Replace variable names with labels and care for a better separation of
  variable and value label of factor variables.
}
\usage{
\method{prettify}{data.frame}(object, labels, sep = ": ", extra.column = FALSE,
                    ...)

\method{prettify}{summary.lm}(object, labels, sep = ": ", extra.column = FALSE,
                    confint = TRUE, level = 0.95,
                    smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                    signif.stars = getOption("show.signif.stars"), ...)

\method{prettify}{summary.glm}(object, labels, sep = ": ", extra.column = FALSE,
                     confint = TRUE, level = 0.95, OR = TRUE,
                     smallest.pval = 0.001, digits = NULL, scientific = FALSE,
                     signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{
    object of class \code{data.frame} resulting (most likely) from a
    call to \code{\link{summary}} or directly a \code{\link{summary}}
    output.
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
  \item{smallest.pval}{
    determines the smallest p-value to be printed
    exactly. Smaller values are given as \dQuote{< smallest.pval}.
    This argument is passed to the \code{eps} argument of
    \code{\link{format.pval}}. See there for details.
  }
  \item{digits}{
    number of significant digits. The default, \code{NULL}, uses
    \code{getOption("digits")}. See also argument \code{digits} in
    \code{\link{format}}.
  }
  \item{scientific}{specifies if numbers should be printed in scientific
    format. For details and possible values see \code{\link{format}}.
  }
  \item{signif.stars}{
    logical. Should significance stars be added? Per default system
    options are used. See \code{ getOption("show.signif.stars")}.
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
  For summary functions additionally confidence
  intervalls (if requested), odds ratio (for logistic regression models,
  if requested), p-values formated for pretty printing and significance
  stars (if requested) are attached.
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

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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}

\keyword{methods}
\keyword{models}

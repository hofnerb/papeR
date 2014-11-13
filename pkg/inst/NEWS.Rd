\name{NEWS}
\title{News for Package 'papeR'}
\section{Changes in papeR version 0.5-0 (2014-11-13)}{
    \subsection{User-visible changes}{
      \itemize{
      \item exported \code{plot.labeled.data.frame} (and some other
      functions for labeled \code{data.frame}s)
      \item greatly improved \code{plot} function for labeled
      \code{data.frame}s 
    }
  }
  \subsection{Miscellaneous}{
    \itemize{
      \item Changed plain text \file{NEWS} to \file{inst/NEWS.Rd}
    }
  }
  \subsection{Bug-fixes}{
    \itemize{
      \item fixed bug in \code{toLatex.sessionInfo(, citations = FALSE)}
      (spotted by Brian S. Diggs <brian.s.diggs __at__ gmail.com>)
    }
  }
}
\section{Changes in papeR version 0.4-0 (2013-09-13)}{
  \itemize{
    \item included (experimental) version of
    \code{prettify.summary.merMod} to make \pkg{lme4} (>= 1.0.0) happy
    \item included better handling of environments in \code{prettify} 
    functions and checks if the data has changed
    \item included possibility to specify confidence intervals directly
    via \code{confint}, e.g.
    
    \code{prettify(summary(mod), confint = confint(mod))}
    
    \item better handling of \code{NAMESPACES} and \code{Imports}
    \item added (first) software tests
  }
}
\section{Changes in papeR version 0.3-0 (2013-08-07)}{
  \itemize{
    \item first release to CRAN
    \item improvement/bugfix in \code{rbind.labeled.data.frame()}
    \item improvement of \code{toLatex.sessionInfo()}:
      file can be \code{NULL} now (no BibTeX file created on HDD)
    \item added plot function for \code{labeled.data.frame} objects that
      allows for univariate plots and grouped, bivariate plots
    \item numerous improvements in manuals
  }
}
\section{Changes in papeR version 0.2-0 (2013-07-01)}{
  \itemize{
    \item greatly improved \code{prettify} functions:
    \itemize{
      \item added prettify functions for \code{summary.coxph},
      \code{summary.lme}, \code{summary.mer} and \code{anova} 
      \item  added confidence intervals to all model based
      \code{prettify} functions and made \code{confint = TRUE} the
      default; confidence intervalls are computed by refitting the
      model within \code{prettify} and using \code{confint()} on this
      model (experimental) 
      \item  improved handling of p-values
    }
    \item added new class \code{"labeled.data.frame"} with special
    \code{subset}, \code{cbind} and \code{rbind} functions (to keep
    labels when changing data set) 
    \item added "shortcut" to define labels using \code{labels =
      TRUE} in \code{latex.tables.XXX}: 
    
    labels are extracted from \code{labeled.data.frame} directly
    
    \item new \code{toLatex} function for \code{sessionInfo()}
    [with nicer results than the default function from base \pkg{R}:
    e.g. citations are included]
  }
}
\section{Changes in papeR version 0.1-0 (2013-01-21)}{
  \itemize{
    \item initial version
    \item improvement of \code{prettify} function
    \item added \code{prettify} functions for more object classes
    \item added function \code{toLatex} to sanitize code for usage in LaTeX
    \item improved label handling when data set is alterd
      (e.g. variables dropped, added or renamed)
  }
}

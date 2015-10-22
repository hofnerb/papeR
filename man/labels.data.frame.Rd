\name{labels}

\alias{labels}
\alias{labels.default}
\alias{labels.data.frame}
\alias{labels<-}

\alias{as.ldf}
\alias{is.ldf}
\alias{convert.labels}

\alias{plot}
\alias{plot.ldf}

\title{
  Extract labels from and set labels for data frames
}
\description{
  Labels can be stored as an attribute \code{"variable.label"} for each
  variable in a data set using the assignment function. With the
  extractor function one can assess these labels.
}
\usage{

\method{labels}{data.frame}(object, which = NULL, abbreviate = FALSE, ...)

## assign labels
labels(data, which = NULL) <- value

## check if data.frame is a special labeled data.frame ('ldf')
is.ldf(object)

## convert object to labeled data.frame ('ldf')
convert.labels(object)
as.ldf(object, ...)

## special plotting function for labeled data.frames ('ldf')
\method{plot}{ldf}(x, variables = names(x),
     labels = TRUE, by = NULL, with = NULL,
     regression.line = TRUE, line.col = "red", ...)
}

\arguments{
  \item{object}{
    a \code{data.frame}.
  }
  \item{data}{
    a \code{data.frame}.
  }
  \item{which}{
    either a number indicating the label to extract or a character
    string with the \emph{variable name} for which the label should be
    extracted. One can also use a vector of numerics or character
    strings to extract mutiple labels. If \code{which} is \code{NULL}
    (default), all labels are returned.
  }
  \item{value}{
    a vector containing the labels (in the order of the variables). If
    which is given, only the corresponding subset is labeled. Note that
    all other labels contain the variable name as label afterwards.
  }
  \item{abbreviate}{
    logical (default: \code{FALSE}). If \code{TRUE} variable labels are
    abbreviated such that they remain unique. See
    \code{\link{abbreviate}} for details. Further arguments to
    \code{\link{abbreviate}} can be specified (see below).
  }
  \item{\dots}{
    further options passed to function \code{\link{abbreviate}} if argument
    \code{abbreviate = TRUE}.

    In \code{x[...]}, \dots can be used to specify indices for
    extraction. See \code{\link{[}} for details.

    In \code{plot}, \code{\dots} can be used to specify further graphial
    parameters.
  }
  \item{x}{
    a labeled \code{data.frame} with class \code{'ldf'}.
  }
  \item{variables}{
    character vector or numeric vector defining (continuous) variables
    that should be included in the table. Per default, all numeric and
    factor variables of \code{data} are used.
  }
  \item{labels}{
    labels for the variables. If \code{labels = TRUE} (the default),
    \code{labels(data, which = variables)} is used as labels. If
    \code{labels = NULL} \code{variables} is used as label.
    \code{labels} can also be specified as character vector.
  }
  \item{by}{
    a character or numeric value specifying a variable in the data set.
    This variable can be either a grouping \code{factor} or is used as
    numeric y-variable (see \code{with} for details). Per default no
    grouping is applied. See also \sQuote{Details} and \sQuote{Examples}.
  }
  \item{with}{
    a character or numeric value specifying a numeric variable
    \code{with} which to \dQuote{correlate} all variables specified in
    \code{variables}. For numeric \code{variables} a scatterplot is
    plotted, for factor \code{variables} one gets a grouped boxplot.
    Per default no variable is given here. Instead of \code{with} one
    can also specify a numeric variable in \code{by} with the same
    results. See also \sQuote{Details} and \sQuote{Examples}.
  }
  \item{regression.line}{
    a logical argument specifying if a regression line should be added
    to scatter plots (which are plotted if both \code{variables} and
    \code{by} are numeric values).
  }
  \item{line.col}{
    the color of the regression line.
  }
}
\details{
  All labels are stored as attributes of the columns of the data frame,
  i.e., each variable has (up to) one attribute which contains the
  variable lable.

  One can set or extract labels from \code{\link{data.frame}} objects.
  If no labels are specified \code{labels(data)} returns the column
  names of the data frame.

  Using \code{abbreviate = TRUE}, all labels are abbreviated to (at
  least) 4 characters such that they are unique. Other minimal lengths
  can specified by setting \code{minlength} (see examples below).

  Univariate plots can be easily obtained for all numeric and factor
  variables in a data set \code{data} by using \code{plot(data)}.

  Bivariate plots can be obtained by specifying \code{by}. In case of a
  factor variable, grouped \code{boxplot}s or \code{spineplot}s are
  plotted depending on the class of the variable specified in
  \code{variables}. In case of a numeric variable, grouped
  \code{boxplot}s or scatter plots are plotted depending on the
  class of the variable specified in \code{variables}. Note that one
  cannot specify \code{by} and \code{with} at the same time (as they are
  internally identical). Note that missings are excluded plot wise (also
  for bivariate plots).
}
\value{
  \code{labels(data)} returns a named vector of variable labels, where
  the names match the variable names and the values represent the labels.
}

\author{
  Benjamin Hofner
}
\note{
  If a data set is generated by \code{\link{read.spss}} in package
  \pkg{foreign}, labels are stored in a single attribute of the data
  set. Assigning new labels, e.g. via \code{labels(data) <-
  labels(data)} removes this attribute and stores all labels as
  attributes of the variables. Alternatively one can use
  \code{data <- convert.labels(data)}.
}
\seealso{
  \code{\link{read.spss}} in package \pkg{foreign}
}
\examples{
############################################################
### Basic labels manipulations

data <- data.frame(a = 1:10, b = 10:1, c = rep(1:2, 5))
labels(data)  ## only the variable names
is.ldf(data) ## not yet

## now set labels
labels(data) <- c("my_a", "my_b", "my_c")
## one gets a named character vector of labels
labels(data)
## data is now a ldf:
is.ldf(data)

## Altervatively one could use as.ldf(data) or convert.labels(data);
## This would keep the default labels but set the class
## correctly.

## set labels for a and b only
## Note that which represents the variable names!
labels(data, which = c("a", "b")) <- c("x", "y")
labels(data)

## reset labels (to variable names):
labels(data) <- NULL
labels(data)

## set label for a only and use default for other labels:
labels(data, which = "a") <- "x"
labels(data)

## attach label for new variable:
data2 <- data
data2$z <- as.factor(rep(2:3, each = 5))
labels(data2)  ## no real label for z, only variable name
labels(data2, which = "z") <- "new_label"
labels(data2)


############################################################
### Abbreviate labels

## attach long labels to data
labels(data) <- c("This is a long label", "This is another long label",
                  "This also")
labels(data)
labels(data, abbreviate = TRUE, minlength = 10)


############################################################
### Data manipulations

## reorder dataset:
tmp <- data2[, c(1, 4, 3, 2)]
labels(tmp)
## labels are kept and order is updated

## subsetting to single variables:
labels(tmp[, 2])  ## not working as tmp[, 2] drops to vector
## note that the label still exists but cannot be extracted
## using labels.default()
str(tmp[, 2])

labels(tmp[, 2, drop = FALSE]) ## prevent dropping

## one can also cbind labeled data.frame objects:
labels(cbind(data, tmp[, 2]))
## or better:
labels(cbind(data, tmp[, 2, drop = FALSE]))
## or rbind labeled.data.set objects:
labels(rbind(data, tmp[, -2]))


############################################################
### Plotting data sets

## plot the data auto"magically"; numerics as boxplot, factors as barplots
par(mfrow = c(2,2))
plot(data2)

## a single plot
plot(data2, variables = "a")
## grouped plot
plot(data2, variables = "a", by = "z")
## make "c" a factor and plot "c" vs. "z"
data2$c <- as.factor(data2$c)
plot(data2, variables = "c", by = "z")
## the same
plot(data2, variables = 3, by = 4)

## plot everithing against "b"
## (grouped boxplots, stacked barplots or scatterplots)
plot(data2, with = "b")
}

\keyword{methods}

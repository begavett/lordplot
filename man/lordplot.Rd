% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lordplot.R
\name{lordplot}
\alias{lordplot}
\title{Create plots in the style of Lord (1953)}
\usage{
lordplot(a = c(0.2, 0.4, 0.6), b = c(-2, -1, 0), c = 0, D = 1.7,
  xlim = c(-4, 4), xby = 0.5, main = NULL, xcol = "lightgrey",
  ycol = "lightblue", xlabcol = xcol, ylabcol = ycol)
}
\arguments{
\item{a}{numeric; item discrimination parameter(s), a vector of length n, where n is the number of test items.}

\item{b}{numeric; item difficulty/location parameter(s), a vector of length n, where n is the number of test items.}

\item{c}{numeric; item guessing parameter(s), either a single value or a vector of length n, where n is the number of test items.}

\item{D}{numeric; constant used to generate results that are roughly equal to the probit scale.}

\item{xlim}{numeric vector of length 2, giving the range of the x coordinates.}

\item{xby}{numeric; gives the size of histogram breaks along the x-axis.}

\item{main}{character; plot title.}

\item{xcol}{the color of the x-axis histogram. The x-axis label also defaults to this color if not specified.}

\item{ycol}{the color of the y-axis histogram. The y-axis label also defaults to this color if not specified.}

\item{xlabcol}{the color of the x-axis label (defaults to xcol).}

\item{ylabcol}{the color of the y-axis label (defaults to ycol).}
}
\description{
This package replicates plots produced by Lord in his 1953 article, 
"The relation of test score to the trait underlying the test." 
This function takes item response theory parameters (currently 2-
and 3-parameter logistic) as the primary inputs and uses them to 
create plots relating a theoretical normal distribution (representing 
the latent trait and plotted on the x-axis) to the "true score" 
distribution (representing a large number of possible observed test 
results and plotted on the y-axis).
}
\examples{
## An example using data and results from the ltm package.
library(ltm)
data(LSAT)
LSAT2PL <- coef(ltm(LSAT ~ z1))
lordplot(a = LSAT2PL[, 2], b = LSAT2PL[, 1])
}
\author{
Brandon E. Gavett, \email{bgavett@uccs.edu}

Richard Jones, \email{richard_jones@brown.edu}
}
\references{
Lord, F. M. (1953). The relation of test score to the trait underlying the test. 
    Educational and Psychological Measurement, 4, 517-549.
}
\keyword{item}
\keyword{response}
\keyword{theory}


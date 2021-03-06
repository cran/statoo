% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.loess.R
\name{plot.loess}
\alias{plot.loess}
\title{Basic plot of a loess object}
\usage{
\method{plot}{loess}(x, ...)
}
\arguments{
\item{x}{An object of class \code{"loess"}.}

\item{...}{Additional graphical arguments.}
}
\description{
Plots a loess object adjusted on one 
unique explanatory variable.
}
\examples{
reg <- loess(dist ~ speed, cars)
plot(reg)

}
\seealso{
\code{\link[stats]{loess}} from package \pkg{stats}.
}


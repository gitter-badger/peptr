% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position-class.R
\name{peptr_is_position}
\alias{peptr_is_position}
\title{Test if an object is of class \code{peptr_position}}
\usage{
peptr_is_position(x)
}
\arguments{
\item{x}{An object.}
}
\value{
\code{TRUE} if object is of class \code{peptr_position} and \code{FALSE} if it is not.
}
\description{
Test if an object is of class \code{peptr_position}.
}
\examples{
x <- peptr_position(1:30)
y <- 1:30

peptr_is_position(x)
peptr_is_position(y)
}
\seealso{
Other position: \code{\link{peptr_position}}
}
\concept{position}

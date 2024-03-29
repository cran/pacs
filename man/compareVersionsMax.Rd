% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{compareVersionsMax}
\alias{compareVersionsMax}
\title{Maximum version across the vector}
\usage{
compareVersionsMax(vec, na.rm = TRUE)
}
\arguments{
\item{vec}{\code{character} vector.}

\item{na.rm}{\code{logical} if to remove \code{NA} values.}
}
\value{
\code{character} maximum version
}
\description{
Reduce function over the \code{utils::compareVersion}
}
\examples{
compareVersionsMax(c("1.1.1", "0.2.0"))
}

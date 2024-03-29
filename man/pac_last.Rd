% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/in_last.R
\name{pac_last}
\alias{pac_last}
\title{Getting the most recent package version}
\usage{
pac_last(pac, repos = biocran_repos())
}
\arguments{
\item{pac}{\code{character} a package name.}

\item{repos}{\code{character} vector of repositories URLs to use. By default checking CRAN and newest Bioconductor per R version. Default \code{pacs::biocran_repos()}}
}
\value{
\code{character} most recent package version.
}
\description{
using \code{utils::available.packages} to get the newest package version.
}
\note{
Results are cached for 30 minutes with \code{memoise} package.
For Bioconductor the newest one per R version.
}
\examples{
\dontrun{
pac_last("dplyr")
pac_last("S4Vectors")
}
}

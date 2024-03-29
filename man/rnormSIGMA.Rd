% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emp_modelling.R
\name{rnormSIGMA}
\alias{rnormSIGMA}
\title{Multivariate normal random series}
\usage{
rnormSIGMA(T, sigma)
}
\arguments{
\item{T}{Length of the generated time series}

\item{sigma}{An (n x n) covariance matrix of the normal series}
}
\value{
T x n matrix of iid normal time series
}
\description{
This function will generate iid multivariate normal random time series.
}
\keyword{internal}

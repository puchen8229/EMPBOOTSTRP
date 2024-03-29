% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emp_modelling.R
\name{MODEL.BOOT}
\alias{MODEL.BOOT}
\title{MODEL.BOOT}
\usage{
MODEL.BOOT(Model, PRP, PRP.PARAM, nrun, Method)
}
\arguments{
\item{Model}{object of a complete Model with known RD. RD="resid" non-parametric bootstrap, RD="norm" parametric bootstrap.}

\item{PRP}{object of the MODEL.PRO output which will be bootstrapped.}

\item{PRP.PARAM}{the parameter that is used to calculate the PRP.}

\item{nrun}{number of bootstrap runs}

\item{Method}{a parameter that determines how the bootstrap samples are generated.}
}
\value{
a list of bootstrapped PARAM, bootstrapped resid and bootstrapped PRP.
}
\description{
This function bootstraps the Properties of a complete Model including bootstrap of the estimated parameters. Often it is used for model validation purpose.
}

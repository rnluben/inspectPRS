% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prs_models.R
\name{prs_models}
\alias{prs_models}
\title{Polygenic risk score modelling functions}
\usage{
prs_models(PRSdata, exposure, outcome, covariates, comparison = NA, nquantiles)
}
\arguments{
\item{PRSdata}{Data frame containing exposure and outcome}

\item{exposure}{Character variable containing name of polygenic risk score variable}

\item{outcome}{Character variable containing name of binary outcome variable}

\item{covariates}{Character vector containing names of covariates}

\item{comparison}{Character variable containing name a secondary PRS}

\item{nquantiles}{Number of quantiles}
}
\value{
A list.
}
\description{
Polygenic risk score modelling functions
}
\examples{
\dontrun{
prs_models(PRSdata,exposure="prs",outcome="disease",covariates= c("age", "sex"),nquantiles=10)
}
}

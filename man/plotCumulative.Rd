% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resultPlots.R
\name{plotCumulative}
\alias{plotCumulative}
\title{Plot specific OMOP results for a given results object and domain}
\usage{
plotCumulative(res, domain, N, conceptDict, cumuDate)
}
\arguments{
\item{res}{a cdm_reference object created using the CDMConnector package}

\item{domain}{The specific OMOP domain to be plotted}

\item{N}{Function will plot the N highest scoring (by count) terms}

\item{conceptDict}{A dictionary of keywords created via makeDict with
attached OMOP concept name and ID information}

\item{cumuDate}{Function will plot the N highest scoring (by count) terms}
}
\description{
Plot specific OMOP results for a given results object and domain
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValueWithUncertainty.R
\name{ValueWithUncertainty}
\alias{ValueWithUncertainty}
\title{ValueWithUncertainty}
\usage{
ValueWithUncertainty(
  LowerCI,
  Value,
  UpperCI,
  model = as.numeric,
  fixed = TRUE,
  name = "default"
)
}
\arguments{
\item{p}{A value to report}

\item{digits}{The number of decimal places}
}
\value{
Returns string of the value \code{p}

Returns p with \code{digits} decimal places

ValueWithUncertainty(1) # returns object
}
\description{
\code{ValueWithUncertainty} returns an object which represents a value with a CI
}

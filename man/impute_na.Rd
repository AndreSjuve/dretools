% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{impute_na}
\alias{impute_na}
\title{Impute missing value based on mean}
\usage{
impute_na(x)
}
\arguments{
\item{x}{Numeric vector}
}
\value{
Returns \code{x} with inserted missing values if one or more of
them have observations on both sides
}
\description{
\code{impute_na} is a function that takes a numeric vector \code{x} and it
inserts the average value of the values on the right and left of the missing
observations. If there is data on both sides a number is returned, if not NA.
}
\details{
Used in scripts: {}
Robust function: FALSE
}

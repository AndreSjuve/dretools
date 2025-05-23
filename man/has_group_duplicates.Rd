% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{has_group_duplicates}
\alias{has_group_duplicates}
\title{Check if your data frame contains duplicate rows by group}
\usage{
has_group_duplicates(data, ...)
}
\arguments{
\item{data}{Dataframe/tibble}

\item{...}{Grouping variables}
}
\value{
TRUE if \code{data} has duplicates, otherwise FALSE
}
\description{
\code{has_group_duplicates} checks if the data frame has duplicate values
when grouping by selection of variables given by \code{...}.
}
\details{
Used in scripts: {}
Robust function: TRUE
}

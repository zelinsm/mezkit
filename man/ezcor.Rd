% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezcor.R
\name{ezcor}
\alias{ezcor}
\title{ezcor: Easily view high correlations in tidy format}
\usage{
ezcor(data, thresh = NULL)
}
\arguments{
\item{data}{A data frame containing variables to check for multicollinearity.}

\item{thresh}{Optional numeric threshold (between 0 and 1). Only variable pairs with absolute correlation greater than this value will be shown. If \code{NULL} (default), all unique correlations are returned.}
}
\value{
A tibble with columns: \code{var1}, \code{var2}, and \code{correlation}, sorted by descending absolute correlation.
}
\description{
Computes pairwise correlations for numeric variables in a data frame and returns
a tidy table of unique variable pairs. Optionally filters for absolute correlation
above a user-specified threshold. Automatically selects only numeric columns and
excludes pairs with missing correlation values.
}
\examples{
ezcor(mtcars)               # View all unique correlations
ezcor(mtcars, thresh = 0.8) # View only high correlations

}

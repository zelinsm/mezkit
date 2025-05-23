% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zap_chars.R
\name{zap_chars}
\alias{zap_chars}
\title{zap_chars: Convert low-cardinality character variables to factors}
\usage{
zap_chars(data, max_levels = 8)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{max_levels}{Maximum number of unique values a character variable can have to be converted to a factor. Default is 8.}
}
\value{
A data frame where qualifying character columns have been converted to factors.
}
\description{
Scans a data frame for character columns and converts those with 8 or fewer unique values
into factors. This is useful for simplifying data cleaning and preparing variables for modeling
or summary tables.
}
\examples{
df <- tibble(
  sex = c("Male", "Female", "Female"),
  id = c("a", "b", "c"),
  diagnosis = c("AD", "MCI", "AD")
)
zap_chars(df)

}

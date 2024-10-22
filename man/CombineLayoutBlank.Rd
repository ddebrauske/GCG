% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CombineLayoutBlank.R
\name{CombineLayoutBlank}
\alias{CombineLayoutBlank}
\title{Combine plate layout and blank}
\usage{
CombineLayoutBlank(layoutDF, blankDF)
}
\arguments{
\item{layoutDF}{is your "long" layout dataframe parsed out from your plate layout matrix}

\item{blankDF}{is the same thing but your blanks}
}
\value{
a dataframe of paired layout and blank information
}
\description{
This function combines the information in platelayout data frame created with PlateLayout and the blank dataframe created with PlateBlank by looping through and matching Coordinate/Plate IDs.
}

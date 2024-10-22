% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeseriesLayoutBlank.R
\name{TimeseriesLayoutBlank}
\alias{TimeseriesLayoutBlank}
\title{Combine information from plate layout and blank reading}
\usage{
TimeseriesLayoutBlank(timepoint.df, layout.blank.df = NULL, layout.df = NULL)
}
\arguments{
\item{timepoint.df}{the dataframe you received from Import() that contains your long, tidy time series data}

\item{layout.blank.df}{use this option if you wish to back-subtract blanks. Use output from CombineLayoutBlank() that contains long, tidy plate layout information}

\item{layout.df}{Use this if you do not wish to back-subtract blanks. Use output from PlateLayout()}
}
\value{
data.combined. a very long, tidy df with all of your data, corrected by back-subtracting the blank values. raw data and value used for back subtracting also provided.
}
\description{
Adds together your dataframes from CombineLayoutBlank and Import to create a very long, tidy dataframe that can be used for plotting and other analysis.
}
\details{
This can be used 2 ways: with or without blank back-subtraction.
}

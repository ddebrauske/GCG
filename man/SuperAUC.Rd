% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SuperAUC.R
\name{SuperAUC}
\alias{SuperAUC}
\title{Super! Area Under The Curve -- AUC summary of many curves}
\usage{
SuperAUC(combined.data, plot = FALSE)
}
\arguments{
\item{combined.data}{data.frame: a tidy,long data with columns "Coordinate", "plate.name", "Time", "OD600", "Strain", "Condition", "Bio_rep" -- from TimeseriesLayoutBlank function}

\item{plot}{logical: would you like to plot the results with ggplot? plots will include biological replicate points plotted with mean and SE. technical reps -identically identified conditiosn within the same "Bio Rep" will be averaged. defaults to FALSE.}
}
\value{
returns a table of AUC values linked to the corresponding wells
}
\description{
takes combined data from TimeseriesLayoutBlank, or data with same format and finds AUC for each curve. Requires my GCG::eAUC function!
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/M003_harmonograph.R
\name{makeHarmonograph}
\alias{makeHarmonograph}
\title{makeHarmonograph}
\usage{
makeHarmonograph(
  frequencies = c(3, 0, 4, 0),
  phases = c(pi/2, 0, 0, 0),
  amplitudes = c(1, 0, 1, 0),
  decay = c(0, 0, 0, 0),
  ...
)
}
\arguments{
\item{...}{parameters passed to makeParametric.}
}
\description{
Functions to calculate a harmonograph
runHarmonograph calculates the points. makeHarmonograph makes a component that calculates and plots the output.
}
\concept{makeParametric
easyPlot(makeHarmonograph())}

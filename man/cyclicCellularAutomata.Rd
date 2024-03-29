% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/G002_cyclicCellularAutomata.R
\name{cyclicCellularAutomata}
\alias{cyclicCellularAutomata}
\alias{runCCA}
\alias{randomInitialCCA}
\alias{makeCCA}
\title{Cyclic Cellular Automata}
\usage{
runCCA(
  input,
  image = NULL,
  numSteps = 10,
  maxNum = 7,
  nbhdType = "VN",
  range = 1,
  threshold = 1
)

randomInitialCCA(maxNum = 7, xSize = 100, ySize = 100)

makeCCA(input = NULL, .abcd = c("a", "d"), ...)
}
\arguments{
\item{input}{a matrix}

\item{image}{a matrix}

\item{numSteps}{The number of iterations of the automata.}

\item{maxNum}{The number of values a cell can have}

\item{nbhdType}{The type of neighbourhood}

\item{range}{the size of the neighbourhood}

\item{threshold}{the requirement to advance a cell}

\item{xSize, ySize}{size of a randomly generated starting grid}

\item{.abcd}{controls which functions are action, build, camera or display. Default is ("a","d").}

\item{...}{parameters passed to the component and the component functions. See "details" for more information.}

\item{.plotting}{logical vector of length two. Used for timing the functions runCCA and matrixToColours.}
}
\value{
makeCCA returns a component. runCCA returns a list containing the image. randomInitialCCA returns a matrix.
}
\description{
Functions to calculate cyclic cellular automata
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F001_polarShapes.R
\name{PolarShapes}
\alias{PolarShapes}
\alias{runPolarShape}
\alias{makePolarShape}
\alias{makeStar}
\alias{makeCircle}
\title{Polar Shapes}
\usage{
runPolarShape(
  x = 0,
  y = 0,
  r = c(0.2, 0.1),
  points = 5,
  loops = 1,
  rotation = 0,
  polarFunction = NULL,
  polarArguments = list(),
  repeats = 1,
  useSegments = FALSE
)

makePolarShape(type = "polygon", ...)

makeStar(...)

makeCircle(r = 1, points = 100, ...)
}
\arguments{
\item{x, y}{the center of the shape}

\item{r}{radius}

\item{points}{the number of points on single loop}

\item{loops}{the number of times the line wraps around the center}

\item{rotation}{rotation in radians. rotates the shape this much.}

\item{polarFunction}{if not null, gives radius instead of r}

\item{polarArguments}{a list of arguments passed to polarFunction}

\item{type}{The name(s) of the plotting function(s) used to plot the component. Defaults to "polygon". This can also include the names of classes linked to a plotting function.}
}
\description{
Functions to draw shapes from polar co-ordinates.
}
\details{
makePolarShape has two modes depending on if a polar function is given instead of a radius r.
}
\examples{
art=makeStar(r=c(0.1,NA,0.2,NA,0.4,NA),points=13,loop=6)
easyPlot(art,preserveAspectRatio=TRUE)
}

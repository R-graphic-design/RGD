% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A021_addColours.R
\name{addColours}
\alias{addColours}
\title{addColours}
\usage{
addColours(
  numberOfColours = 10,
  scheme = "BW",
  colours = NULL,
  bias = 1,
  space = c("rgb", "Lab"),
  alpha = FALSE,
  interpolate = c("linear", "spline"),
  reverse = FALSE,
  discrete = TRUE,
  randomise = FALSE,
  addColoursName = ".col"
)
}
\arguments{
\item{numberOfColours}{The number of colours returned. Defaults to 10.}

\item{scheme}{The palette or function to use. Used when colours is NULL. Defaults to "BW" for black and white. See details for more information.}

\item{colours}{A starting set of colours that will be interpolated.}

\item{bias}{used by colorRampPalette}

\item{space}{used by colorRampPalette}

\item{alpha}{used by colorRampPalette}

\item{interpolate}{used by colorRampPalette}

\item{reverse}{Reverses the final colour order. Defaults to FALSE}

\item{discrete}{Defaults to TRUE}

\item{randomise}{logical. If TRUE randomises the order of the answer. Defaults to FALSE}

\item{addColoursName}{the name of the parameter in the returned list.}
}
\value{
Returns a named list containing the vector of colours of the given length.
}
\description{
A wrapper for colourScheme that can be added to components/layers etc using code like +action.data("addColours").
}
\details{
The possible values of scheme are;
\itemize{
\item "BW" and "BWrev" for greyscale
\item "rainbow" and "rainbowrev" which uses rainbow()
\item "heat" and "heatrev" which uses heat.colors()
\item "terrain" and "terrainrev" which uses terrain.colors()
\item "topo" and "toporev" which uses topo.colors()
\item "cm" and "cmrev" which uses cm.colors()
\item "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
"RdYlGn","Spectral","Accent","Dark2","Paired","Pastel1","Pastel2","Set1",
"Set2","Set3","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
"YlOrBr" and "YlOrRd" from the RColorBrewer package
\item "plasma", "plasmarev", "viridis", "viridisrev", "magma", "magmarev", "inferno" and "infernorev" from the viridis package.
}

*"BottleRocket1","BottleRocket2","Rushmore1","Rushmore",
"Royal1","Royal2","Zissou1","Darjeeling1",
"Darjeeling2","Chevalier1","FantasticFox1","Moonrise1",
"Moonrise2","Moonrise3","Cavalcanti1","GrandBudapest1",
"GrandBudapest2","IsleofDogs1" and "IsleofDogs2" from the wesanderson package.

The above terms that end in rev overrides the reverse parameter to force reversing the colour scheme.
}
\examples{
addColours()
}

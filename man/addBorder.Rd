% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/E001_addBorder.R
\name{addBorder}
\alias{addBorder}
\title{addBorder}
\usage{
addBorder(
  art,
  extra = 0.1,
  res = NULL,
  foreground = "black",
  mode = 0,
  useGrid = FALSE,
  ...
)
}
\arguments{
\item{art}{an artwork object}

\item{extra}{The size of the border}

\item{res}{The resolution of the artwork}

\item{foreground}{colour of line around the border}

\item{mode}{the mode used in the artwork NEEDED?????}

\item{useGrid}{logical needed??}

\item{...}{parameters are passed to the background component}
}
\value{
returns an artwork object. The sections of the original are inside new sections that also contain borders.
}
\description{
Adds a border to an artwork object.
}
\details{
creates two additional components, a background colour and a foreground border. Both are rect
}
\examples{
print(1+1)
}

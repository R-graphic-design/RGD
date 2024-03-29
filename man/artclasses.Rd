% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D004_functionclasses.R
\docType{class}
\name{artclasses}
\alias{artclasses}
\alias{initialize,artclasses-method}
\alias{artClasses}
\alias{+,artwork,artclasses-method}
\alias{-,artwork,artclasses-method}
\title{artclasses}
\usage{
\S4method{initialize}{artclasses}(.Object, ..., classList = as.character())

artClasses(...)

\S4method{+}{artwork,artclasses}(e1, e2)

\S4method{-}{artwork,artclasses}(e1, e2)
}
\arguments{
\item{...}{extra items to be added to the classList slot}

\item{classList}{a list passed to the classList slot}
}
\description{
An S4 class for storing class-function associations.
}
\details{
This class is a convenient way of adding classes to artwork objects.
}
\section{Slots}{

\describe{
\item{\code{classList}}{a list of class-function associations}
}}

\examples{
print(1+1)
}

% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/grid.R
\name{runBMLGrid}
\alias{runBMLGrid}
\title{Takes an existing BML grid and moves the cars according to specified number of steps (old version)}
\usage{
runBMLGrid(a, numSteps)
}
\arguments{
\item{a}{BML grid}

\item{numSteps}{Number of steps}
}
\value{
BML grid
}
\description{
Takes an existing BML grid and moves the cars according to specified number of steps (old version)
}
\examples{
a=createBMLGrid(10,10,c(30,40))
runBMLGrid(a,13)
}


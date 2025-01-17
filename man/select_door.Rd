% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{select_door}
\alias{select_door}
\title{Simulate a contestant's selection of the first door.}
\usage{
select_door()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns an integer between 1 and 3.
}
\description{
\code{select_door} utilizes 'sample' to select a number between 1 and 3
that would correspond with the number of doors available to the contestant
to choose from.
}
\details{
This function is used to represent the contestants first choice of 1 door
out of the 3 available doors presented.
}
\examples{
select_door()
}

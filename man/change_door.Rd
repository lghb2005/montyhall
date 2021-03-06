% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{change_door}
\alias{change_door}
\title{Change the door.}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{stay}{logical vector, default to true.}

\item{opened.door}{numeric vectors, length 1, between 1 and 3, cannot
equal to a.pick.}

\item{a.pick}{numeric vectors, length 1, between 1 and 3, cannot
equal to opened.door.}
}
\value{
The function returns a length 1 numeric vector
indicating the contestant's final selection.
}
\description{
\code{change_door()} returns the final contestant's decision on the door
position.
}
\details{
The contestant is given the option to change from their initial
selection to the other door that is still closed. This function first
creates a vector of doors numbered 1, 2, 3. Then if the contestant
decides to stay, the function will return their final selection of the
position on the door to open, which is the same as their initial choice.
If the contestant switches, the returned final selection from this
function would be the door that has not yet been opened nor has been not
selected by the contestant.
}
\examples{
  change_door( T, 1, 2 )
  change_door( F, 1, 2 )
  change_door( T, 2, 3 )
  change_door( F, 2, 3 )

}

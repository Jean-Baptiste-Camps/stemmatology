\name{expandVL}
\alias{expandVL}
\title{Expand a variant location table with alternative readings}
\description{
Takes in entry a variant location table with alternate readings and outputs a table with atomic readings, by creating new rows for every possible combination of readings.
}
\usage{
expandVL(x)
}
\arguments{
  \item{x}{a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings).}
}
\details{This function is used internally to provide the option \code{alternateReadings}, but it can also be used directly to create a matrix with atomic readings, to be used with various functions.}
\value{
a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a single number.}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{Jean-Baptiste Camps}
\note{
This function can output a very large matrix, according to the number of possible combinations.
}

\seealso{
\code{\link{PCC}}, \code{\link{PCC.Exploratory}},
\code{\link{PCC.conflicts}}, \code{\link{PCC.contam}}, \code{\link{PCC.distribute}}
}
\examples{
# Create a simple matrix
x = matrix(c("1,2", "1,2,3", "2,3", "2", "0,2"), 
    ncol = 5, dimnames = list("VL1", c("A", "B", "C", "D", "E")))
# Apply the function
y = expandVL(x)
# Look for conflicting information
PCC.conflicts(y)
}
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{stemmatology}

\encoding{utf8}
\name{Import.TEIApparatus}
\alias{Import.TEIApparatus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import TEI apparatus}
\description{Allows the user to import a TEI encoded parallel-segmentation apparatus.}
\usage{
Import.TEIApparatus(file = "")
}

\arguments{
  \item{file}{
Path to a valid TEI file..
}
}
\details{
This function takes in input the path to a TEI file, with a \code{<listWit>} and with \code{app} entries encoded in parallel-segmentation mode. Using the witness sigla, it then creates a database of variant locations, with witnesses in columns and variant locations in rows. 
}

\value{%
Either a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
or, if alternative readings were found at some point, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings).
The output of this function can be used as input for the \code{\link{PCC}} functions.
}
%\references{
%}
\note{
If you want more control over the conversion, you can use directly the stylesheets available at \url{https://github.com/Jean-Baptiste-Camps/stemmatology-utils}.
}

\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}

\keyword{manip}
\keyword{stemmatology}

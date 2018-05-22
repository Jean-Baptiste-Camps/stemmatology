\encoding{utf8}
\name{import.TEIApparatus}
\alias{import.TEIApparatus}
\title{Import TEI apparatus}
\description{Import a TEI encoded parallel-segmentation apparatus.}
\usage{
import.TEIApparatus(file = "", appTypes = c("substantive"))
}

\arguments{
  \item{file}{Path to a valid TEI file.}
  \item{appTypes}{a vector of strings giving the types of entries that should be retained (as per \code{app/@type}). Default: "substantive". To include all types and
  no type, set to \code{NULL}.}
}
\details{
This function takes in input the path to a TEI file, with a \code{<listWit>}, with \code{<witness>} identified by an \code{@xml:id}, 
and with \code{<app>} entries encoded in parallel-segmentation mode. Using the witness sigla, it then creates a database of variant locations, with witnesses in columns and variant locations in rows. 
The \code{@types} attributes of the \code{<app>} elements are used to assess if they should be included in the variant locations matrix (default: only `substantive' app entries).
The readings are identified either by a code
reflecting their order in the file (1 … n) and omissions by 0. If \code{<app>} entries have \code{@xml:id}, they are used as rownames. Otherwise, the index is used.
}

\value{%
Either a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
or, if alternative readings were found at some point, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings).
The output of this function can be used as input for the \code{\link{PCC}} functions.
}

\references{
Jean-Baptiste Camps, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2), edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://hal.archives-ouvertes.fr/hal-01695903/document}.
}

\note{
If you want more control over the conversion, you can use directly the stylesheets available at \url{https://github.com/Jean-Baptiste-Camps/stemmatology-utils}.
}

\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}

\keyword{manip}
\keyword{stemmatology}

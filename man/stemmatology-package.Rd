\encoding{utf8}
\name{stemmatology}
\alias{stemmatology-package}
\docType{package}
\title{An R stemmatology package}
\description{
This packages implements various methods for the stemmatological analysis of textual traditions. It contains both exploratory methods and algorithms aimed at building stemmata from a table of variant locations.
}
\details{
\tabular{ll}{
Package: \tab stemmatology\cr
Type: \tab Package\cr
Version: \tab 0.2.2\cr
Date: \tab 2014-07-16\cr
License: \tab  CC BY-NC-SA 2.0\cr
}

This package includes various functions for stemmatological analysis. 

\subsection{Input}{

Most of the functions take, as input a \emph{numeric matrix}, with witnesses in columns, variant locations in rows, and readings coded by a number, e.g.

\tabular{lllllllllll}{
  \tab A \tab B \tab C \tab D \tab E \tab H \tab I \tab J \tab K \tab O\cr
1 \tab 0 \tab 1 \tab 1 \tab 1 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
2 \tab 1 \tab 1 \tab 1 \tab 1 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
3 \tab 1 \tab 1 \tab 1 \tab 1 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
4 \tab 1 \tab 1 \tab 1 \tab 2 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
5 \tab 1 \tab 1 \tab 1 \tab 2 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
6 \tab 1 \tab 1 \tab 1 \tab 1 \tab NA \tab 1 \tab 1 \tab NA \tab 1 \tab 1\cr
}
where A, B, …, O are the various witnesses in columns, 1…6 the various variant locations, in rows, and the differents readings are coded either 0 (omission), 1, 2, …, n. NA is used for the lack of information (physical lacuna, absence of observation, variant location not applicable to a given witness, etc.).

Alternatively , if \code{alternateReadings = TRUE}, the input can be a \emph{character matrix}, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings), e.g.

\tabular{llllll}{
 \tab A \tab D1 \tab F \tab T \tab P1 \cr
1 \tab 1 \tab 2 \tab 2 \tab 2 \tab 1,2 \cr
2 \tab 1 \tab 2 \tab 1,2 \tab 2 \tab 1 \cr
3 \tab 1 \tab 1 \tab 1 \tab 1 \tab 2 \cr
4 \tab 1,3 \tab 1,2 \tab 1 \tab 2 \tab 3 \cr
}
Notice how a witness can bear several readings (e.g., P1 at VL 1).
}

% Import functions
\subsection{Import functions}{
    \code{\link{Import.TEIApparatus}}: a function to import a TEI P5 encoded apparatus into a stemmatological matrix usable with other functions. It should have options for import from CollateX. %TODO: est-ce implémenté ?
}
% Fonctions 

\subsection{PCC Method}{

    Functions are made available for the PCC method (See Camps and Cafiero 2014 or \code{\link{PCC}} for more details). The most important are
    
    \code{\link{PCC}}: global shell for the PCC functions
    
    \code{\link{PCC.Exploratory}}: global function for exploratory methods of the PCC family
    
    \code{\link{PCC.Stemma}}: Building the Stemma Codicum.
    }

\subsection{Other functions}{
The package contains also various other functions, particularly aimed at
detecting contamination. It contains for instance the function \code{\link{VL.pValues}} (presented in Camps 2013 unpublished)

% Other methods
The package aims at making available various other stemmatological methods, including further functions for contamination detection, 
or for theoretical stemmatology.

%The package should include a function to generate arbitrary stemmata based on a set of parameters (fecundity, decimation rate at each level, number of generations, ...).

%It should also have a function for the analysis of stemmata shapes, and making hypotheses on the parameters of the original tradition
}    
    
}

\author{
Jean-Baptiste Camps (École nationale des chartes, PSL Research University).

Florian Cafiero.

Maintainer: Jean-Baptiste Camps <\email{jbcamps@hotmail.com}>.
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.
%TODO: mettre le poster quand paru.
}

\note{%
Please report issues with this package to \url{https://github.com/Jean-Baptiste-Camps/stemmatology}.
}

\keyword{stemmatology}
\keyword{stemmatics}
\keyword{stemma}

\seealso{
\code{\link{PCC}},
\code{\link{PCC.Exploratory}}, \code{\link{PCC.Stemma}}
}

\examples{
data(fournival)
%PCC.Exploratory(fournival)
}

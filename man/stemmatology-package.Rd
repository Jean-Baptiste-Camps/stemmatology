\encoding{utf8}
\name{stemmatology}
\alias{stemmatology-package}
\docType{package}
\title{An R Stemmatology Package}
\description{
Build and analyse the genealogy of textual or musical traditions.
}
\details{
\tabular{ll}{
Package: \tab stemmatology\cr
Type: \tab Package\cr
Version: \tab 0.3.0\cr
Date: \tab 2018-05-20\cr
License: \tab GPL-3\cr
}

\emph{Stemmatology} is the name of the field dedicated to studying text genealogies
and establishing genealogical tree-like graphs known as stemma codicum.

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
where A, B, …, O are the various witnesses in columns, 1…6 the various variant locations, in rows, and the different readings are coded either 0 (omission), 1, 2, …, \emph{n}. \code{NA} is used for the lack of information (physical lacuna, absence of observation, variant location not applicable to a given witness, etc.).

Alternatively , if \code{alternateReadings = TRUE}, the input can be a \emph{character matrix}, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings), e.g.

\tabular{llllll}{
 \tab A \tab D \tab F \tab T \tab P \cr
1 \tab "1" \tab "2" \tab "2" \tab "2" \tab "1,2" \cr
2 \tab "1" \tab "2" \tab "1,2" \tab "2" \tab "1" \cr
3 \tab "1" \tab "1" \tab "1" \tab "1" \tab "2" \cr
4 \tab "1,3" \tab "1,2" \tab "1" \tab "2" \tab "3" \cr
}
Notice how a witness can bear several readings (e.g., P at VL 1).
}

% Import functions
\subsection{Create or import data}{
  Data can be created inside R or imported. They can be imported by reading a csv file, for instance (e.g. with \code{\link{read.csv}}). They can also be imported from a TEI encoded apparatus in parallel-segmentation, either by using an XSL stylesheet, or the built-in function \code{\link{import.TEIApparatus}}.
  
  The function \code{\link{import.TEIApparatus}} allows to import a TEI P5 encoded apparatus into a stemmatological matrix usable with other functions. It has some parameters to refine the import (variant types, …), and can read either from disk or from an URL. 
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
detecting contamination. It contains for instance the function \code{\link{PCC.contam}}.%\code{\link{VL.pValues}} (presented in Camps 2013 unpublished)

% Other methods
The package aims at making available various other stemmatological methods, including further functions for contamination detection, 
or for theoretical stemmatology.

%The package should include a function to generate arbitrary stemmata based on a set of parameters (fecundity, decimation rate at each level, number of generations, ...).

%It should also have a function for the analysis of stemmata shapes, and making hypotheses on the parameters of the original tradition
}    
    
}

\author{
Jean-Baptiste Camps (École nationale des chartes | Université PSL).

Florian Cafiero.

Maintainer: Jean-Baptiste Camps <\email{jbcamps@hotmail.com}>.
}

\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.
}

\note{%
Please report issues with this package to \url{https://github.com/Jean-Baptiste-Camps/stemmatology}.
}

\keyword{stemmatology}

\seealso{
\code{\link{PCC}},
\code{\link{PCC.Exploratory}}, \code{\link{PCC.Stemma}}
}

\examples{

# Load data
data(fournival)
# or alternatively, import it
#fournival = import.TEIApparatus(file = "myFournival.xml", 
#    appTypes = c("substantive"))

\dontrun{
# Interactive mode

# Analyse it with the PCC functions
PCC(fournival)
}

# Complete step-by-step non interactive use
data("fournival")
\dontshow{
# subsetting for exec time
fournival = fournival[1:200,c(-5,-8)]
}
# look for conflicts
myConflicts = PCC.conflicts(fournival)
# remove conflicting VL
myConflicts = PCC.overconflicting(myConflicts, ask = FALSE, threshold = 0.06)
myNewData = PCC.elimination(myConflicts)
# look for competing genealogies
myConflicts = PCC.conflicts(myNewData)
myNewData = PCC.equipollent(myConflicts, ask = FALSE, scope = "W", wits = "D")
# build a stemma
PCC.Stemma(myNewData$databases[[1]], ask = FALSE)
}

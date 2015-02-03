\name{stemmatology}
\alias{stemmatology-package}
\docType{package}
\title{A R Stemmatology package
What the package does (short line)
~~ package title ~~
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab stemmatology\cr
Type: \tab Package\cr
Version: \tab 0.2.2\cr
Date: \tab 2014-07-16\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
This package includes various functions for stemmatological analysis.

% Import functions
\subsection{Import functions}{
    \code{\link{Import.TEIApparatus}}: a function to import a TEI P5 encoded apparatus into a stemmatological matrix usable with other functions. It should have options for import from CollateX and from Juxta.
}
% Fonctions 

\subsection{PCC Method}{
    Functions are made available for the PCC method (See Camps and Cafiero 2014...), the most important are
    
    \code{\link{PCC}}: global shell for the PCC functions
    
    \code{\link{PCC.Exploratory}}
    
    \code{\link{PCC.Stemma}}
}

% Exploratory methods
Methods to explore the possible proofs of contamination inside a textual tradition
\code{\link{VL.pValues}}

% Other methods
The package aims at making available various other stemmatological methods ...

% Tools
Various tools for stemmatological analysis

The package should include a function to generate arbitrary stemmata based on a set of parameters (fecundity, decimation rate at each level, number of generations, ...).

It should also have a function for the analysis of stemmata shapes, and making hypotheses on the parameters of the original tradition

}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
Jean-Baptiste Camps And Florian Cafiero, « Genealogical Variant Locations And Simplified Stemma: A Test Case », 2014
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}

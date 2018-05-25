\encoding{utf8}
\name{PCC.Exploratory}
\alias{PCC.Exploratory}
\title{PCC Exploratory methods}
\description{
This is the global function for exploratory methods of the PCC family.  
It interactively makes use of the lower-level exploratory functions, to assess conflicts between variant locations, eliminate problematic configurations or identify likely contaminations. 
}
\usage{
PCC.Exploratory(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, 
    pauseAtPlot = FALSE, ask = TRUE, threshold = NULL, verbose = FALSE)
}
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings). 
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings (and taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).%; if \code{FALSE}, they are treated only as omissions.  
     Default: \code{FALSE}.
}
  \item{alternateReadings}{
  logical; if \code{TRUE}, a witness can have multiple variants for a single variant location (contaminated manuscripts, editio variorum, …), encoded as comma-separated values. %; if \code{FALSE}, only one reading per witness is allowed. 
  Default: \code{FALSE}.
  }
  \item{pauseAtPlot}{logical; if \code{TRUE}, the algorithms stops at each plot during the execution of \code{PCC.contam} (by setting graphical parameter \code{ask = TRUE}). Default: \code{FALSE}.
}
  \item{ask}{
  logical; if \code{FALSE}, decisions will be made without asking the user for input.
  Default: \code{TRUE}
  }
  \item{threshold}{numeric; the centrality threshold above which variant locations are considered to be over-conflicting. Used only with \code{ask = FALSE}.}
  \item{verbose}{
  logical; if \code{FALSE}, the function will only return the results, without information on the operations.
  Default: \code{FALSE}.
  }
}
\details{
This function is meant to guide the user through the process of assessing and eliminating unreliable variant locations and/or identify competing genealogies (i.e. contamination), as described in Camps & Cafiero 2015. 

It starts by computing and plotting the network of conflicting variant locations (i.e. variant locations that present contradictory genealogical information), by calling \code{\link{PCC.conflicts}}, and then interactively aids the user in determining overconflicting variant locations (with \code{\link{PCC.overconflicting}}), eliminating problematic variant locations (\code{\link{PCC.elimination}}), detecting contamination (\code{\link{PCC.contam}}) or creating new databases reflecting competing genealogies (\code{\link{PCC.equipollent}}).
}
\value{
According to the choices made by the user, the output can be an object belonging to one of the following classes: \code{pccConflicts}, \code{pccOverconflicting}, \code{pccContam} or \code{pccEquipollent}.
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.

Poole, Eric. ‘L’analyse stemmatique des textes documentaires’. \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151-161.

Poole, Eric, ‘The Computer in Determining Stemmatic Relationships’. \emph{Computers and the Humanities}, 8-4 (1974), p. 207-16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}

\seealso{
\code{\link{PCC.conflicts}}, \code{\link{PCC.overconflicting}}, \code{\link{PCC.elimination}}, \code{\link{PCC.contam}}, \code{\link{PCC.equipollent}}.
}
\examples{
# Load data
data(fournival)
\dontrun{
# Interactive mode
# or alternatively, import it
#fournival = import.TEIApparatus(file = "myFournival.xml", 
#                      appTypes = c("substantive"))
# Analyse it with the PCC functions
PCC.Exploratory(fournival)
}
\dontshow{
# subsetting for exec time
fournival = fournival[1:50,c(-5,-8)]
}
# Non interactive mode
PCC.Exploratory(fournival, ask = FALSE, threshold = 0.06)
}
\keyword{stemmatology}

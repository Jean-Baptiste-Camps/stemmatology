\encoding{utf8}
\name{PCC.Stemma}
\alias{PCC.Stemma}
\title{PCC.Stemma: Building the Stemma Codicum}
\description{Builds a \emph{stemma codicum} of the tradition, following the Poole-Camps-Cafiero method.
}
\usage{
PCC.Stemma(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs= TRUE,
           layout_as_stemma = FALSE, ask = TRUE, verbose = FALSE)
}
\arguments{
  \item{x}{a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number; or a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings).
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
     
     Default: \code{FALSE}.
}
  \item{limit}{
The maximum number of severe disagreements expected for witnesses to be in the same group. 

    Default: \code{0}.
}
 \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. 
  Default: \code{TRUE}.
}
\item{layout_as_stemma}{logical; if TRUE, the witnesses will be placed vertically
according to the distance from their parent, as per the function \code{\link{layout_as_stemma}} (experimental!)
Default: FALSE
}
\item{ask}{
  logical; if \code{FALSE}, decisions will be made without asking the user for input.
  Default: \code{TRUE}
  }
  \item{verbose}{
  logical; if \code{FALSE}, the function will only return the results, without information on the operations.
  Default: \code{FALSE}.
  }
}
\details{
The \code{PCC.Stemma} function calls successively the functions \code{\link{PCC.disagreement}}, \code{\link{PCC.buildGroup}} and \code{\link{PCC.reconstructModel}} to build a stemma codicum of the tradition studied. By default, it stops when less than four witnesses are to be compared, as the possibility of errors becomes high. The user is however able to ask the algorithm its final answer for those last witnesses.
}
\value{The function returns either a single list, or a list containing several lists (if multiple stemmata were drawn). Each list contains:
\item{fullDatabase}{The full database, with the new reconstructed models 
  and recovered NAs (if applicable).}
  \item{database}{The same with the \emph{descripti} removed.}
  \item{edgelist}{An edgelist expressing the relations between the witnesses of each group with, as a third column, the distances between witnesses.}
  \item{models}{A list containing the database of readings for each model at the time of their reconstruction (i.e., before they are compared to extant witnesses).}
  \item{modelsByGroup}{A matrix with, in columns the groups, and a single row containing the label of their model.}
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.

Poole, Eric. ‘L’analyse stemmatique des textes documentaires’. \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151-161.

Poole, Eric, ‘The Computer in Determining Stemmatic Relationships’. \emph{Computers and the Humanities}, 8-4 (1974), p. 207-16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero (\email{florian.cafiero@polytechnique.edu})
}
\seealso{
\code{\link{PCC.disagreement}}, \code{\link{PCC.buildGroup}}, \code{\link{PCC.reconstructModel}}, \code{\link{layout_as_stemma}}.
}
\examples{
# Load data
data("parzival")
# or alternatively, import it
#fournival = import.TEIApparatus(file = "myFournival.xml", 
#                      appTypes = c("substantive"))
\dontrun{
# Interactive mode
# Analyse it with the PCC functions
myDBs = PCC.Exploratory(parzival)
# draw a stemma
PCC.Stemma(myDBs$databases[[2]])
}
\dontshow{
# subsetting for exec time
parzival = parzival[1:100,c(-6,-8,-12,-13,-14)]
}
# Non interactive mode
myDBs = PCC.Exploratory(parzival, ask = FALSE, threshold = 0.06)
PCC.Stemma(myDBs$databases[[2]], ask = FALSE)
}
\keyword{stemmatology}

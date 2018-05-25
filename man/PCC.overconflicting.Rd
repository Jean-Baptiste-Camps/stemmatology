\encoding{utf8}
\name{PCC.overconflicting}
\alias{PCC.overconflicting}
\title{PCC Exploratory Methods: Identification of Over-Conflicting Variant Locations}
\description{
Given a network of conflicts (contradictions) between variant locations, this function helps in assessing which are the problematic ones.
}
\usage{
PCC.overconflicting(x, ask = TRUE, threshold = NULL)
}
\arguments{
  \item{x}{an object of class pccConflicts.}
  \item{ask}{
  logical; if \code{FALSE}, decisions will be made without asking the user for input.
  Default: \code{TRUE}
  }
  \item{threshold}{numeric; the centrality threshold above which variant locations are considered to be over-conflicting. Used only with \code{ask = FALSE}.}
}
\details{
This function is dedicated to the identification of problematic variant locations, as defined in Poole 1974 and Camps & Cafiero 2015. It helps the user defining a threshold, defined in terms of centrality index, above which variant locations are considered to be over-conflicting. This output can be then passed to the function \code{\link{PCC.elimination}}, to remove them from the database.
}
\value{
  An object of class "pccOverconflicting", a list containing the three same first objects as the "pccConflicts" input, 
  \item{edgelist}{a two-column character matrix, giving the edges between variant locations in the network of conflicts}
  \item{conflictsTotal}{a one-column numeric matrix, giving the total number of conflicts per variant location}
  \item{database}{the original database used for the calculations}
  and adding
  \item{vertexAttributes}{a two column character matrix, with a row per vertex of the network (i.e. variant location), giving its label and colour}
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
\code{\link{PCC.Exploratory}},
\code{\link{PCC.conflicts}},
\code{\link{PCC.elimination}}.
}
\examples{
# Load data
data("fournival")
\dontshow{
# subsetting for time gain
fournival = fournival[1:25,]
}
# Analyse its conflicts
myConflicts = PCC.conflicts(fournival)
\dontrun{
# Interactive mode: identify over-conflicting VL
PCC.overconflicting(myConflicts)
}
# Non interactive mode
PCC.overconflicting(myConflicts, ask = FALSE, threshold = 0.06)
}
\keyword{stemmatology}

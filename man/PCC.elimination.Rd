\encoding{utf8}
\name{PCC.elimination}
\alias{PCC.elimination}
\title{PCC Exploratory Methods: Elimination of Over-Conflicting Variant Locations}
\description{
This function removes from the database the variant locations labelled as over-conflicting by the \code{PCC.overconflicting} function.
}
\usage{
PCC.elimination(x)
}
\arguments{
  \item{x}{an object of class pccOverconflicting.}
}
\details{
When PCC.overconflicting has been applied to a PCC.conflicts object, it returns a table where over-conflicting variants are labeled as such. The PCC.elimination function simply removes those variants.
}
\value{
A numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number, from which over-conflicting variant locations have been removed. 
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
The notion of using a centrality threshold for the identification of over-conflicting variant locations is found in Camps & Cafiero 2015. Other formulas for this centrality might be implemented in the future.
}

\seealso{
\code{\link{PCC.Exploratory}},
\code{\link{PCC.conflicts}},
\code{\link{PCC.overconflicting}},
\code{\link{PCC.contam}}.
}
\examples{
# Load data
data("fournival")
# Analyse its conflicts
myConflicts = PCC.conflicts(fournival)
\dontrun{
# Interactive mode: identify over-conflicting VL
PCC.overconflicting(myConflicts)
}
# Non interactive mode
myConflicts = PCC.overconflicting(myConflicts, ask = FALSE, threshold = 0.06)
# Create a new DB without problematic VL
myNewDB = PCC.elimination(myConflicts)
}
\keyword{stemmatology}

\encoding{utf8}
\name{PCC.disagreement}
\alias{PCC.disagreement}
\title{PCC.disagreement: Find disagreements and agreements between witnesses}
\description{
  The PCC.disagreement function helps spotting disagreements (and agreements) between manuscripts. For a given numeric matrix, representing the variants in different manuscripts, it locates disagreements (benign or severe), agreements and omissions in common between manuscripts.}
\usage{
PCC.disagreement(x, omissionsAsReadings = FALSE)
}

\arguments{
  \item{x}{
 a numeric matrix, with manuscripts in columns, variants in rows, and readings coded by a number.
}
  \item{omissionsAsReadings}{
     logical; if TRUE, omissions are considered as readings.
}
}
\details{
A distinction is made between severe and benign disagreements (see Camps & Cafiero 2015). Severe disagreements are disagreements between witnesses on two readings that are each shared by at least two witnesses. They have stronger genealogical implications than benign disagreements, that involve at least one singular reading. This distinction is used by the methods of the \code{\link{PCC}} family.

This function also gives common omissions, and oriented omissions (i.e. omission present in one witness but not an other). No distinction is made between omission and addition, as this means establishing the orientation in genealogical relationship.

Agreements are given as well, mostly with an indicative value, as they cannot be taken as a direct measure of similarity.
}
\value{
The function returns:
  \item{database }{The original database.}
  \item{severeDisagreement }{A list of the severe disagreements between manuscripts.}
  \item{benignDisagreement }{A list of the benign disagreements between manuscripts.}
  \item{agreements}{A list of agreements between manuscripts.}
  \item{omissionsInCommon }{A list of all the omissions in common between manuscripts(if omissionsAsReadings is set to TRUE, this will be NA).}
  \item{omissionsOriented }{A list of the omissions present in a manuscript but not in another (if omissionsAsReadings is set to TRUE, this will be NA).}

}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.

Poole, Eric. ‘L’analyse stemmatique des textes documentaires’. \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151-161.

Poole, Eric, ‘The Computer in Determining Stemmatic Relationships’. \emph{Computers and the Humanities}, 8-4 (1974), p. 207-16.
}
\author{Jean-Baptiste Camps & Florian Cafiero}

\seealso{
\code{\link{PCC.Stemma}}, \code{\link{PCC.buildGroup}}, \code{\link{PCC.reconstructModel}}.
}
\examples{
#Load a tradition
data("fournival")
#Option: explore the tradition to see problems in variant locations
#PCC.Exploratory(fournival)
\dontshow{
# subsetting for time gain
fournival = fournival[1:25,]
}
#Calculate disagreements
PCC.disagreement(fournival)
}
\keyword{stemmatology}

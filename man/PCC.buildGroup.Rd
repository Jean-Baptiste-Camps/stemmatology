\encoding{utf8}
\name{PCC.buildGroup}
\alias{PCC.buildGroup}
\title{PCC.buildGroup: Group Witnesses in Clusters}
\description{
\code{PCC.buildGroup} groups together witnesses in relevant clusters, based on the absence (or number inferior to a limit) of severe disagreements between them.
}
\usage{
PCC.buildGroup(x, limit = 0, ask = TRUE)
}
\arguments{
  \item{x}{
A PCC.disagreement object.
}
  \item{limit}{
  The maximum number of severe disagreements allowed for two witnesses in the same group. Default (and advised) value: \code{0}.
  }
  \item{ask}{
  logical; if FALSE, decisions will be made without asking the user for input.
  Default: TRUE
  }
}
\details{
  Witnesses a number of severe disagreements between them lesser than or equal to \code{limit} are grouped together. This disagreement-based method is described in Camps & Cafiero 2015.
}
\value{
The function returns a list containing:
  \item{database }{The original database.}
  \item{groups }{A list of the groups that were created, identified by their labels.}

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
\code{\link{PCC.Stemma}}, \code{\link{PCC.disagreement}}, \code{\link{PCC.reconstructModel}}.
}
\examples{
# A fictional simple tradition
x = matrix(
    c(
      1,0,1,1,1,1,1,1,
      1,0,1,2,2,2,1,2,
      1,0,0,3,2,1,NA,3,
      2,0,1,4,NA,1,1,1,
      2,1,2,5,2,1,1,4
    ), nrow = 8, ncol = 5,
    dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7","VL8"),
                    c("A","B","C","D","E")))
# Compute disagreement(s)
x = PCC.disagreement(x)
# And now build the groups
PCC.buildGroup(x)
}
\keyword{stemmatology}

\name{PCC.conflicts}
\alias{PCC.conflicts}
\title{PCC Exploratory Methods: Conflicts between Variant Locations}
\description{
Given a matrix of variant locations, this function compares them by pairs to identify contradictions between them.
}
\usage{
PCC.conflicts(x, omissionsAsReadings = FALSE, alternateReadings = FALSE)
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
}
\details{
This function tries to identify conflicts between variant locations, understood as contradictions in the genealogical information they might contain. In order to do that, every possible pair of variant locations is analysed in order to see if both can denote at least one possible normal genealogy (i.e. a genealogy without contamination or polygenesis). If not, they are considered "conflicting". 

A network representing all the conflicts between variant locations is drawn, and the total number of conflicts and centrality index by variant location is given,
as an help to estimate which variant locations are unreliable. This output can be then passed to the function \code{\link{PCC.overconflicting}}.
See Camps & Cafiero 2015 for more details.
}
\value{
An object of class "pccConflicts", a list containing
  \item{edgelist}{a two-column character matrix, giving the edges between variant locations in the network of conflicts (adjacency list)}
  \item{conflictsTotal}{a one-column numeric matrix, giving the total number of conflicts per variant location}
  \item{database}{the original database used for the calculations}
}

\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{PCC.Exploratory}}, \code{\link{PCC.overconflicting}}.
}
\examples{
%TODO: fignoler globalement les exemples et les reprendre, voire les affiner.
load(fournival)
myConflicts = PCC.conflicts(fournival)
myOverconflicting = PCC.overconflicting(myConflicts)
myNewVariantsTable = PCC.elimination(myOverconflicting)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}
\keyword{philology}
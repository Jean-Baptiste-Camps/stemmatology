\encoding{utf8}
\name{PCC.overconflicting}
\alias{PCC.overconflicting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC Exploratory Methods: Identification of Over-Conflicting Variant Locations}
\description{
Given a network of conflicts (contradictions) between variant locations, this function helps in assessing which are the problematic ones.
}
\usage{
PCC.overconflicting(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class pccConflicts.}
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
  \item{vertexAttributes}{a two column character matrix, with a row per vertex of the network (i.e. variant location), giving its label and color}
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
\code{\link{PCC.Exploratory}},
%About the input for this fonction : 
\code{\link{PCC.conflicts}},
%To remove the variants labeled as problematic: 
\code{\link{PCC.elimination}}.
}
\examples{
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}
\keyword{philology}

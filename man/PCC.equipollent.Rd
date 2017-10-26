\encoding{utf8}
\name{PCC.equipollent}
\alias{PCC.equipollent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCC Exploratory Methods: Studying Competing Genealogies
}
\description{
Some over-conflicting variants can be algorithmically ruled out for the building of a stemma (see \code{PCC.overconflicting} and \code{PCC.elimination}). Yet, in some cases, choosing between conflicting variables is algorithmically undecidable. PCC.equipollent helps addressing this case. It either replicates the database to examine the different possble cases, either creates one database where witnesses probably contaminated are replicated.
}
\usage{
PCC.equipollent(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{PCC.conflicts} object.
}
}
\details{

}
\value{
An object of class pccEquipollent, a list containing
    \item{databases}{a list with all alternative databases that have been created, if any}
    \item{notInConflict}{a list with The group(s) of VL without internal conflicts}
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
For the input: \code{\link{PCC.conflicts}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}
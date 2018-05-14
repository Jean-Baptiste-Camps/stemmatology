\encoding{utf8}
\name{PCC.elimination}
\alias{PCC.elimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC Exploratory Methods: Elimination of Over-Conflicting Variant Locations}
\description{
This function removes from the database the variant locations labeled as over-conflicting by the \code{PCC.overconflicting} function.
}
\usage{
PCC.elimination(x)
}
%- maybe also 'usage' for other objects documented here.
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
\code{\link{PCC.conflicts}},
\code{\link{PCC.overconflicting}},
\code{\link{PCC.contam}}.
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}

\name{PCC.doElimination}
\alias{PCC.doElimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
The PCC.doElimination function removes the variant locations labeled as over-conflicting by the PCC.elimination function.
}
\usage{
PCC.doElimination(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A PCC.elimination object.
}
}
\details{
When PCC.elimination has been applied to a PCC.conflict object, it returns a table where over-conflicting variants are labeled as such. The PCC.doElimination function simply removes those variants.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Jean-Baptiste Camps et Florian Cafiero, "Genealogical variant locations and simplified stemma: a test case", in Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches, dir. Tara Andrews & Caroline Mace, Turnhout, 2015, p. 69-93 (Lectio, 1)

Eric Poole, "The Computer in Determining Stemmatic Relationships", Computers and the Humanities 8-4, 1974, p. 207-16
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
To identify and label over-conflicting variants: \code{\link{PCC.elimination}}

To spot the conflicts between variants: \code{\link{PCC.conflicts}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{over-conflicting}
\keyword{variants}% __ONLY ONE__ keyword per line
\keyword{selection}
\keyword{deletion}
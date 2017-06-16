\name{PCC.elimination}
\alias{PCC.elimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
PCC.elimination is dedicated to the elimination of problematic variant locations, as defined in Poole (1974) and Camps & Cafiero (2015). It helps the user defining a threshold above which variant locations are considered to be over-conflicting. It returns a table where those variant locations are labeled as problematic.
}
\usage{
PCC.elimination(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a PCC.conflict object
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Jean-Baptiste Camps et Florian Cafiero, "Genealogical variant locations and simplified stemma: a test case", in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Mace, Turnhout, 2015, p. 69-93 (Lectio, 1)

Eric Poole, "The Computer in Determining Stemmatic Relationships", \emph{Computers and the Humanities} 8-4, 1974, p. 207-16
 
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
To remove the variants labeled as problematic: \code{\link{PCC.doElimination}}

About the input for this fonction : \code{\link{PCC.conflicts}
}
\examples{
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{over-conflicting}
\keyword{variants}% __ONLY ONE__ keyword per line

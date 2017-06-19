\name{PCC.overconflicting}
\alias{PCC.overconflicting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
PCC.overconflicting is dedicated to the identification of problematic variant locations, as defined in Poole (1974) and Camps & Cafiero (2015). It helps the user defining a threshold above which variant locations are considered to be over-conflicting. It returns a table where those variant locations are labeled as overconflicting.
}
\usage{
PCC.overconflicting(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class pccConflicts}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%About the input for this fonction : 
\code{\link{PCC.conflicts}},
%To remove the variants labeled as problematic: 
\code{\link{PCC.elimination}},
\code{\link{PCC.Exploratory}}
}
\examples{
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{over-conflicting}
\keyword{variants}% __ONLY ONE__ keyword per line

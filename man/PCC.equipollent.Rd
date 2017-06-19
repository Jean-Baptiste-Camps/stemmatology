\name{PCC.equipollent}
\alias{PCC.equipollent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
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
%% ~put references to the literature/web site here ~
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
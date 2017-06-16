\name{PCC.equipollent}
\alias{PCC.equipollent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Some over-conflicting variants can be algorithmically ruled out for the building of a stemma (see \code{PCC.overconflicting} and \code{PCC.elimination}). Yet, in some cases, choosing between conflicting variables is algorithmically undecidable. PCC.equipollent helps addressing this case. It either replicates the data base to examine the different possble cases, either creates one database where witnesses probably contaminated are replicated.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{PCC.conflicts}
\alias{PCC.conflicts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
PCC.conflicts aims at detecting conflicts between variants.
}
\usage{
PCC.conflicts(x, omissionsAsReadings = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a numeric matrix, with witnesses in colums, variants in rows, and readings coded by a number.
}
  \item{omissionsAsReadings}{
  logical; if TRUE, omissions are considered as readings.
}
  \item{alternateReadings}{
  logical; if TRUE, allows for alternate readings in the same variant location of a manuscript.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class "pccConflicts", a list containing
  \item{edgelist}{a two-column character matrix, giving the edges between variant locations in the network of conflicts}
  \item{conflictsTotal}{a one-column numeric matrix, giving the total number of conflicts per variant location}
  \item{database}{the original database used for the calculations}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemmatology}
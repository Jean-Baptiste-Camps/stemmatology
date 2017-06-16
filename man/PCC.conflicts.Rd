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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{variants}
\keyword{conflicts}% __ONLY ONE__ keyword per line
\keyword{detection}
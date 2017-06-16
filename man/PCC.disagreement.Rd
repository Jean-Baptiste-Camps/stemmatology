\name{PCC.disagreement}
\alias{PCC.disagreement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
  The PCC.disagreement function helps spotting disagreements (and agreements) between manuscripts. For a given numeric matrix, representing the variants in different manuscripts, it locates disagreements (benigne or severe), agreements and omissions in common between manuscripts.
}
\usage{
PCC.disagreement(x, omissionsAsReadings = FALSE)
}

\arguments{
  \item{x}{
 a numeric matrix, with manuscripts in colums, variants in rows, and readings coded by a number.
}
  \item{omissionsAsReadings}{
     logical; if TRUE, omissions are considered as readings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns:
  \item{severeDisagreement }{A list of the severe disagreements between manuscripts.}
  \item{benigneDisagreement }{A list of the benigne disagreements between manuscripts.}
  \item{agreements}{A list of agreements between manuscripts.}
  \item{omissionsInCommon }{A list of all the omissions in common between manuscripts.}
  \item{omissionsOriented }{A list of the omissions present in a manuscript but not in another.}

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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

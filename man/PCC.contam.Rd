\name{PCC.contam}
\alias{PCC.contam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
This function proposes a test to detect a possible contamination. It computes the number of conflicting variants when removing one of the witnesses, for each witness. If removing a witness makes the number of conflicting variants significantly drop, then contamination can be considered as plausible.
}
\usage{
PCC.contam(x, pauseAtPlot = FALSE, omissionsAsReadings = FALSE)
}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pauseAtPlot}{
logical;  if TRUE, the algorithm stops each time it builds a plot.
}
  \item{omissionsAsReadings}{
logical; if TRUE, omissions are considered as readings.
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

\section{Warning }{The execution of this command can be time-consuming for large databases.}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{contamination}
\keyword{detection}% __ONLY ONE__ keyword per line

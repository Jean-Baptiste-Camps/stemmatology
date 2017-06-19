\name{PCC.Stemma}
\alias{PCC.Stemma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.Stemma(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs= TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{

}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
     
     Default: \code{FALSE}.
}
  \item{limit}{
The maximum number of severe disagreements expected for witnesses to be in the same group. 

    Default: \code{0}.
}
 \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. 
  
  Default: \code{TRUE}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%% TODO(JBC): définir et documenter la class "pccStemma" de sortie de cette fonction.
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
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
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
\keyword{stemma}

\name{incrementTable}
\alias{incrementTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
incrementTable(tableToIncrement, tableVariantes, x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tableToIncrement}{
%%     ~~Describe \code{tableToIncrement} here~~
}
  \item{tableVariantes}{
%%     ~~Describe \code{tableVariantes} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tableToIncrement, tableVariantes, x, y) 
{
    if (is.na(tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]])) {
        tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]] = 1
    }
    else {
        tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]] = tableToIncrement[colnames(tableVariantes)[x], 
            colnames(tableVariantes)[y]] + 1
    }
    return(tableToIncrement)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

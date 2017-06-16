\name{PCC.doElimination}
\alias{PCC.doElimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
 ~~ The PCC.doElimination function removes the variant locations labeled as over-conflicting by the PCC.elimination function. ~~
}
\usage{
PCC.doElimination(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   ~~ A PCC.elimination object. ~~
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
~~ \code{\link{PCC.elimination}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    overconflicting = NULL
    for (i in 1:nrow(x$vertexAttributes)) {
        if (x$vertexAttributes[i, 1] == "overconflicting") {
            overconflicting = c(overconflicting, rownames(x$vertexAttributes)[i])
        }
    }
    x$database = x$database[!rownames(x$database) \%in\% overconflicting, 
        , drop = FALSE]
    return(x$database)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

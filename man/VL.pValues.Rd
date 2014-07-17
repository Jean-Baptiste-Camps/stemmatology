\name{VL.pValues}
\alias{VL.pValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VL.pValues(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    tableVariantes = as.matrix(x, dimnames = labels(x))
    tableVariantes[is.na(tableVariantes)] = 0
    pValueTable = matrix(nrow = nrow(tableVariantes), ncol = nrow(tableVariantes), 
        dimnames = c(labels(tableVariantes)[1], labels(tableVariantes)[1]))
    totalpValue = matrix(nrow = nrow(tableVariantes), ncol = 1, 
        dimnames = c(labels(tableVariantes)[1], "Total inf\303\251rieur \303\240 0,05"))
    for (i in 1:nrow(tableVariantes)) {
        testTable = tableVariantes[-i, , drop = FALSE]
        totalpValue[i, ] = 0
        for (j in 1:nrow(testTable)) {
            fisherResult = fisher.test(tableVariantes[i, ], testTable[j, 
                ])
            pValue = fisherResult$p.value
            pValueTable[rownames(tableVariantes)[i], rownames(testTable)[j]] = pValue
            if (pValue < 0.051) {
                totalpValue[i, ] = totalpValue[i, ] + 1
            }
        }
    }
    pValueTable = cbind(pValueTable, totalpValue)
    courbeSignificativite = apply(pValueTable, MARGIN = 1, mean, 
        na.rm = TRUE)
    plot(courbeSignificativite, type = "l")
    return(pValueTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

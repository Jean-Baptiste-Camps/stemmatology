\name{PCC.distribute}
\alias{PCC.distribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.distribute(x)
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
    if (!is.matrix(x) && !is.character(x)) {
        stop("The imput database is not a character matrix. If it is a numeric matrix (i.e. not containing alternate readings), this function is unnecessary.")
    }
    tableVariantes = as.matrix(x)
    toBeRemoved = NULL
    myNewRows = list(NULL)
    myNewRownames = NULL
    for (i in 1:nrow(tableVariantes)) {
        myAlternates = grep(",", tableVariantes[i, ], fixed = TRUE)
        if (length(myAlternates) > 0) {
            toBeRemoved = c(toBeRemoved, i)
            myRow = as.list(tableVariantes[i, ])
            for (j in 1:length(myAlternates)) {
                myRow[myAlternates[j]] = strsplit(myRow[[myAlternates[j]]], 
                  split = ",", fixed = TRUE)
            }
            rowsToAdd = expand.grid(myRow, stringsAsFactors = FALSE)
            myNewRownames = c(myNewRownames, paste(rownames(tableVariantes)[i], 
                "-", 1:nrow(rowsToAdd), sep = ""))
            myNewRows = mapply(c, myNewRows, rowsToAdd, SIMPLIFY = FALSE)
        }
    }
    if (!is.null(toBeRemoved) && !is.null(myNewRows)) {
        tableVariantes = tableVariantes[-toBeRemoved, , drop = FALSE]
        tableVariantes = rbind(tableVariantes, matrix(unlist(myNewRows), 
            ncol = ncol(tableVariantes), dimnames = list(myNewRownames, 
                NULL)))
        mode(tableVariantes) = "numeric"
    }
    return(tableVariantes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

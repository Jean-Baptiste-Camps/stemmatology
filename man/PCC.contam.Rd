\name{PCC.contam}
\alias{PCC.contam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.contam(x, pauseAtPlot = FALSE, omissionsAsReadings = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pauseAtPlot}{
%%     ~~Describe \code{pauseAtPlot} here~~
}
  \item{omissionsAsReadings}{
%%     ~~Describe \code{omissionsAsReadings} here~~
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
function (x, pauseAtPlot = FALSE, omissionsAsReadings = FALSE) 
{
    if (pauseAtPlot == TRUE) {
        par(ask = TRUE)
    }
    else {
        par(ask = FALSE)
    }
    tableVariantes = x$database
    conflictsDifferences = as.data.frame(character(0))
    X = character(0)
    X = as.list(X)
    X$totalByMs = x$conflictsTotal
    for (i in 1:ncol(tableVariantes)) {
        database = tableVariantes[, -i, drop = FALSE]
        pccConflicts = PCC.conflicts(database, omissionsAsReadings = omissionsAsReadings)
        if (length(pccConflicts$edgelist) != 0) {
            legend("topright", c("Without ms. ", colnames(tableVariantes)[i]))
        }
        else {
            message = paste("Without ms. ", colnames(tableVariantes)[i], 
                "there is NO CONFLICTS in the database. If the number of conflicts is \n otherwise high, it is likely that this manuscript is in cause")
            writeLines(message)
            if (pauseAtPlot == TRUE) {
                cat("Press [enter] to continue")
                line = readline()
            }
        }
        difference = (sum(pccConflicts$conflictsTotal[1])/2) - 
            (sum(x$conflictsTotal[1])/2)
        conflictsDifferences = rbind(conflictsDifferences, difference)
        rownames(conflictsDifferences)[i] = colnames(tableVariantes)[i]
        colnames(pccConflicts$conflictsTotal)[1] = colnames(tableVariantes)[i]
        X$totalByMs = cbind(X$totalByMs, pccConflicts$conflictsTotal)
        diffMSTotal = pccConflicts$conflictsTotal[1]
        for (j in 1:nrow(X$totalByMs)) {
            diffMSTotal[j, ] = pccConflicts$conflictsTotal[j, 
                1] - X$totalByMs[j, 1]
        }
        colnames(diffMSTotal) = "Difference"
        X$totalByMs = cbind(X$totalByMs, diffMSTotal)
    }
    X$conflictsDifferences = conflictsDifferences
    colnames(X$conflictsDifferences) = "Conflicts differences"
    par(ask = FALSE)
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

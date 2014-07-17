\name{PCC.desagreement}
\alias{PCC.desagreement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.desagreement(x, omissionsAsReadings = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x, omissionsAsReadings = FALSE) 
{
    tableVariantes = as.matrix(x)
    severeDesagreement = matrix(nrow = ncol(tableVariantes), 
        ncol = ncol(tableVariantes), dimnames = c(labels(tableVariantes)[2], 
            labels(tableVariantes)[2]))
    benigneDesagreement = matrix(nrow = ncol(tableVariantes), 
        ncol = ncol(tableVariantes), dimnames = c(labels(tableVariantes)[2], 
            labels(tableVariantes)[2]))
    agreements = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsInCommon = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsOriented = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    for (i in 1:nrow(tableVariantes)) {
        for (j in 1:ncol(tableVariantes)) {
            if (is.na(tableVariantes[i, j])) {
            }
            else {
                if ((tableVariantes[i, j] == 0) && (omissionsAsReadings == 
                  FALSE)) {
                  for (k in 1:ncol(tableVariantes)) {
                    if (is.na(tableVariantes[i, k]) == FALSE) {
                      if (is.na(omissionsInCommon[colnames(tableVariantes)[j], 
                        colnames(tableVariantes)[k]])) {
                        omissionsInCommon[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]] = 0
                      }
                      if (is.na(omissionsOriented[colnames(tableVariantes)[j], 
                        colnames(tableVariantes)[k]])) {
                        omissionsOriented[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]] = 0
                      }
                      if (tableVariantes[i, j] == tableVariantes[i, 
                        k]) {
                        omissionsInCommon = incrementTable(omissionsInCommon, 
                          tableVariantes, j, k)
                      }
                      if (tableVariantes[i, j] < tableVariantes[i, 
                        k]) {
                        omissionsOriented = incrementTable(omissionsOriented, 
                          tableVariantes, j, k)
                      }
                    }
                  }
                }
                else {
                  if (is.numeric(tableVariantes[i, j])) {
                    for (k in 1:ncol(tableVariantes)) {
                      if ((is.na(tableVariantes[i, k]) == FALSE) && 
                        ((tableVariantes[i, k] != 0) | (omissionsAsReadings == 
                          TRUE))) {
                        if (is.na(agreements[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          agreements[colnames(tableVariantes)[j], 
                            colnames(tableVariantes)[k]] = 0
                        }
                        if (is.na(severeDesagreement[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          severeDesagreement[colnames(tableVariantes)[j], 
                            colnames(tableVariantes)[k]] = 0
                        }
                        if (is.na(benigneDesagreement[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          benigneDesagreement[colnames(tableVariantes)[j], 
                            colnames(tableVariantes)[k]] = 0
                        }
                        if (tableVariantes[i, j] == tableVariantes[i, 
                          k]) {
                          agreements = incrementTable(agreements, 
                            tableVariantes, j, k)
                        }
                        if (tableVariantes[i, j] != tableVariantes[i, 
                          k]) {
                          tableSansjk = tableVariantes[i, -c(j, 
                            k), drop = FALSE]
                          if ((tableVariantes[i, j] \%in\% tableSansjk) && 
                            (tableVariantes[i, k] \%in\% tableSansjk)) {
                            severeDesagreement = incrementTable(severeDesagreement, 
                              tableVariantes, j, k)
                          }
                          else {
                            benigneDesagreement = incrementTable(benigneDesagreement, 
                              tableVariantes, j, k)
                          }
                        }
                      }
                    }
                  }
                  else {
                    errorMessage = paste("You have non numeric values in the database at [", 
                      i, ",", j, "] aborting calculation.")
                    print(errorMessage)
                    break
                  }
                }
            }
        }
    }
    X = character(0)
    X = as.list(X)
    X$database = tableVariantes
    X$severeDesagreement = severeDesagreement
    X$benigneDesagreement = benigneDesagreement
    X$agreements = agreements
    X$omissionsInCommon = omissionsInCommon
    X$omissionsOriented = omissionsOriented
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

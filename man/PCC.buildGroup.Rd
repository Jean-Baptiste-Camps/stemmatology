\name{PCC.buildGroup}
\alias{PCC.buildGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.buildGroup(x, limit = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
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
function (x, limit = 0) 
{
    severeDesagreement = x$severeDesagreement
    if (length(severeDesagreement[is.na(severeDesagreement)]) > 
        0) {
        print("There are NA values in your severeDesagreement matrix")
        answered = FALSE
        writeLines("Do you whish to proceed anyway (careful !) ?")
        while (answered == FALSE) {
            answer = readline("(Y/N)")
            if (answer != "N" && answer != "Y") {
                print("Please enter Y (yes) or N (no).")
            }
            if (answer == "N") {
                return()
            }
            if (answer == "Y") {
                severeDesagreement[is.na(severeDesagreement)] = 0
                answered = TRUE
            }
        }
    }
    groups = as.list(NULL)
    for (i in 1:nrow(severeDesagreement)) {
        groups[[i]] = labels(severeDesagreement[i, severeDesagreement[i, 
            ] == limit])
    }
    toBeRemoved = as.vector(NULL)
    for (j in 1:(length(groups) - 1)) {
        for (k in (j + 1):length(groups)) {
            if (identical(groups[[j]], groups[[k]])) {
                toBeRemoved = c(toBeRemoved, k)
            }
        }
    }
    if (!is.null(toBeRemoved)) {
        groups = groups[-toBeRemoved]
    }
    toBeRemovedAsWell = as.vector(NULL)
    for (l in 1:length(groups)) {
        if ((length(groups[[l]]) == 1) && (groups[[l]] == 1)) {
            toBeRemovedAsWell = c(toBeRemovedAsWell, l)
        }
        if (length(groups[[l]]) > 2) {
            problems = 0
            for (m in 1:(length(groups[[l]]) - 1)) {
                for (n in (m + 1):length(groups[[l]])) {
                  if (severeDesagreement[groups[[l]][m], groups[[l]][n]] > 
                    0) {
                    problems = problems + 1
                  }
                }
            }
            if (problems > 0) {
                message = paste("there is a weird configuration ; we will remove this group from the list alltogether. It concerns mss:")
                print(message)
                print(groups[[l]])
                toBeRemovedAsWell = c(toBeRemovedAsWell, l)
            }
        }
    }
    if (!is.null(toBeRemovedAsWell)) {
        groups = groups[-toBeRemovedAsWell]
    }
    output = as.list(NULL)
    output$database = x$database
    output$groups = groups
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

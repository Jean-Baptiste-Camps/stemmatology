\name{PCC.equipollent}
\alias{PCC.equipollent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.equipollent(x)
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
    myNetwork = as.network(x$edgelist, directed = FALSE, matrix.type = "edgelist")
    adjacencyTable = as.matrix.network(myNetwork)
    database = x$database
    notInConflict = as.list(NULL)
    for (i in 1:nrow(adjacencyTable)) {
        notInConflict[[i]] = labels(adjacencyTable[i, adjacencyTable[i, 
            ] == 0])
    }
    toBeRemoved = as.vector(NULL)
    for (j in 1:(length(notInConflict) - 1)) {
        for (k in (j + 1):length(notInConflict)) {
            if (identical(notInConflict[[j]], notInConflict[[k]])) {
                toBeRemoved = c(toBeRemoved, k)
            }
        }
    }
    if (!is.null(toBeRemoved)) {
        notInConflict = notInConflict[-toBeRemoved, drop = FALSE]
    }
    toBeRemovedAsWell = as.vector(NULL)
    for (l in 1:length(notInConflict)) {
        if (length(notInConflict[[l]]) > 2) {
            problems = 0
            for (m in 1:(length(notInConflict[[l]]) - 1)) {
                for (n in (m + 1):length(notInConflict[[l]])) {
                  if (adjacencyTable[notInConflict[[l]][m], notInConflict[[l]][n]] > 
                    0) {
                    problems = problems + 1
                  }
                }
            }
            if (problems > 0) {
                message = paste("there is a weird configuration ; we will remove this group from the list alltogether.\n It concerns VL:")
                writeLines(message)
                print(notInConflict[[l]])
                toBeRemovedAsWell = c(toBeRemovedAsWell, l)
            }
        }
    }
    if (!is.null(toBeRemovedAsWell)) {
        notInConflict = notInConflict[-toBeRemovedAsWell, drop = FALSE]
    }
    print("The group of VL without internal conflicts are :")
    print(notInConflict)
    writeLines("Do you wish to separate them for the whole tradition [T]\n or only for some (presumably contaminated) manuscripts [M]\n - or not at all (press any other key)?")
    answer = readline("(T/M/q)")
    if ((answer == "T") | (answer == "M")) {
        databases = as.list(NULL)
        if (answer == "T") {
            for (o in 1:length(notInConflict)) {
                delete = rownames(adjacencyTable)[!rownames(adjacencyTable) \%in\% 
                  notInConflict[[o]]]
                databases[[o]] = database[!rownames(database) \%in\% 
                  delete, , drop = FALSE]
            }
        }
        if (answer == "M") {
            mss = unlist(strsplit(readline("Please enter the manuscript siglum, or manuscripts sigla (space separated list) \n"), 
                "\\s+", perl = "TRUE"))
            for (o in 1:length(notInConflict)) {
                delete = rownames(adjacencyTable)[!rownames(adjacencyTable) \%in\% 
                  notInConflict[[o]]]
                databases[[o]] = database
                databases[[o]][rownames(database) \%in\% delete, 
                  mss] = NA
            }
        }
        return(databases)
    }
    else {
        return(notInConflict)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

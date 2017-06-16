\name{PCC.conflicts}
\alias{PCC.conflicts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
~~ PCC.conflicts aims at detecting conflicts between variants. ~~
}
\usage{
PCC.conflicts(x, omissionsAsReadings = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 ~~ a numeric matrix, with manuscripts in colums, variants in rows, and readings coded by a number. ~~
}
  \item{omissionsAsReadings}{
     ~~ logical; if TRUE, omissions are considered as readings.~~
}
  \item{alternateReadings}{
     ~~ logical; if TRUE, allows for alternate readings in the same variant location of a manuscript.~~
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
    if (omissionsAsReadings == FALSE) {
        tableVariantes[tableVariantes == 0] = NA
    }
    edgelist = matrix(c(character(0), character(0)), ncol = 2)
    conflictsTotal = as.data.frame(c(character(0)))
    for (i in 1:nrow(tableVariantes)) {
        conflictsTotal = rbind(conflictsTotal, 0)
    }
    row.names(conflictsTotal) = rownames(tableVariantes)
    totalVL = nrow(tableVariantes)
    for (i in 1:(totalVL - 1)) {
        VLA = i
        supVLA = VLA + 1
        if (!supVLA > totalVL) {
            for (j in supVLA:totalVL) {
                VLB = j
                factorVLA = as.factor(tableVariantes[VLA, ])
                factorVLB = as.factor(tableVariantes[VLB, ])
                problematicConfiguration = FALSE
                interactions = interaction(factorVLA:factorVLB, 
                  drop = TRUE)
                if (length(grep(",", interactions)) > 1) {
                  toBeSplitted = grep(".*,.*", interactions, 
                    value = TRUE)
                  interactions = as.factor(grep(".*,.*", interactions, 
                    value = TRUE, invert = TRUE))
                  factorVLA = as.factor(grep(".*,.*", factorVLA, 
                    value = TRUE, invert = TRUE))
                  factorVLB = as.factor(grep(".*,.*", factorVLA, 
                    value = TRUE, invert = TRUE))
                  toBeSplitted = strsplit(toBeSplitted, ":")
                  for (z in toBeSplitted) {
                    N1 = strsplit(z[1], ",")
                    N2 = strsplit(z[2], ",")
                    combinations = expand.grid(N1[[1]], N2[[1]])
                    factorVLA = factor(c(as.character(factorVLA), 
                      as.character(combinations[, 1])))
                    factorVLB = factor(c(as.character(factorVLB), 
                      as.character(combinations[, 2])))
                    newInteractions = interaction(combinations, 
                      sep = ":")
                    interactions = factor(c(as.character(interactions), 
                      as.character(newInteractions)))
                  }
                }
                levelsInteractions = levels(interactions)
                if (length(levelsInteractions) > 3) {
                  levelsVLA = levels(factorVLA)
                  lengthLevelsVLA = length(levelsVLA)
                  testCounter = 0
                  while (problematicConfiguration == FALSE && 
                    testCounter < (lengthLevelsVLA - 1)) {
                    for (k in levelsVLA) {
                      searchForK = paste(k, ":(.+)", sep = "")
                      if (length(grep(searchForK, levelsInteractions, 
                        ignore.case = TRUE, perl = TRUE)) > 1) {
                        secondNumbers = regexec(searchForK, levelsInteractions, 
                          ignore.case = TRUE)
                        secondNumbers = do.call(rbind, lapply(regmatches(levelsInteractions, 
                          secondNumbers), `[`))
                        levelsSecondNumbers = levels(as.factor(secondNumbers[, 
                          2]))
                        invertMatches = grep(searchForK, levelsInteractions, 
                          ignore.case = TRUE, perl = TRUE, value = TRUE, 
                          invert = TRUE)
                        alternativeFirstNumbers = regexec("(.+?):.+", 
                          levels(as.factor(invertMatches)), ignore.case = TRUE)
                        alternativeFirstNumbers = do.call(rbind, 
                          lapply(regmatches(levels(as.factor(invertMatches)), 
                            alternativeFirstNumbers), `[`))
                        levelsAlternativeFirstNumbers = levels(as.factor(alternativeFirstNumbers[, 
                          2]))
                        for (l in levelsAlternativeFirstNumbers) {
                          problems = 0
                          for (m in levelsSecondNumbers) {
                            searchForProblem = paste(l, ":", 
                              m, sep = "")
                            if (length(grep(searchForProblem, 
                              invertMatches, ignore.case = TRUE, 
                              perl = TRUE)) > 0) {
                              problems = problems + 1
                            }
                            if (problems > 1) {
                              problematicConfiguration = TRUE
                            }
                          }
                        }
                      }
                      testCounter = testCounter + 1
                    }
                  }
                }
                if (problematicConfiguration == TRUE) {
                  edgelist = rbind(edgelist, c(rownames(tableVariantes)[VLA], 
                    rownames(tableVariantes)[VLB]))
                  conflictsTotal[VLA, ] = conflictsTotal[VLA, 
                    ] + 1
                  conflictsTotal[VLB, ] = conflictsTotal[VLB, 
                    ] + 1
                }
            }
        }
    }
    centrality = conflictsTotal
    if (sum(conflictsTotal) > 0) {
        sumConflicts = sum(conflictsTotal)/2
        for (z in 1:nrow(centrality)) {
            centrality[z, ] = centrality[z, ]/(sumConflicts - 
                centrality[z, ])
            if (is.infinite(centrality[z, ])) {
                centrality[z, ] = 2
            }
        }
    }
    else {
        for (z in 1:nrow(centrality)) {
            centrality[z, ] = centrality[z, ] = 0
        }
    }
    X = NULL
    X = as.list(X)
    X$edgelist = edgelist
    X$conflictsTotal = cbind(conflictsTotal, centrality)
    colnames(X$conflictsTotal) = c("Number of conflicts", "Centrality index")
    X$database = x
    if (length(edgelist) != 0) {
        myNetwork = as.network(edgelist, directed = FALSE, matrix.type = "edgelist")
        gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork), 
            gmode = "graph", boxed.labels = TRUE)
    }
    else {
        print("There is absolutely no conflicts in this database.")
    }
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

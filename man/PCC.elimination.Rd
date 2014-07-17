\name{PCC.elimination}
\alias{PCC.elimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.elimination(x)
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
    reseau = x
    ordConflTot = reseau$conflictsTotal[order(reseau$conflictsTotal[, 
        1], decreasing = TRUE), ]
    testClasses = ordConflTot[, 1]
    if (length(testClasses[test > 0]) > 3) {
        numberOfClasses = 4
    }
    else {
        print("The number of conflicts is VERY LOW. Is your database correct?")
        numberOfClasses = 3
    }
    classes1 = pam(ordConflTot[1], numberOfClasses)
    barplot(ordConflTot[, 1], col = classes1$clustering, main = "Total de conflits par lieu variant", 
        names.arg = rownames(ordConflTot), xlab = "VL", ylab = "Conflicts Total", 
        ylim = (c(0, ordConflTot[1, 1])), cex.axis = "1", sub = "coloured according to partitioning around medoids with 4 clusters", 
        yaxt = "n")
    axis(side = 2, at = seq(0, ordConflTot[1, 1], by = 2))
    par(ask = TRUE)
    classes2 = pam(ordConflTot[2], numberOfClasses)
    barplot(ordConflTot[, 2], col = classes2$clustering, main = "Indice de centralit\303\251 par lieu variant", 
        names.arg = rownames(ordConflTot), xlab = "VL", ylab = "Centrality Index", 
        ylim = (c(0, ordConflTot[1, 2])), cex.axis = "1", sub = "coloured according to partitioning around medoids with 4 clusters", 
        yaxt = "n")
    axis(side = 2, at = seq(0, ordConflTot[1, 2], by = 0.02))
    myNetwork = as.network(reseau$edgelist, directed = FALSE, 
        matrix.type = "edgelist")
    answered = FALSE
    while (answered == FALSE) {
        seuilCentrality = as.numeric(readline("Choisissez le seuil de centralit\303\251 > "))
        if (is.na(seuilCentrality)) {
            print("Please enter a number.")
        }
        else {
            if (seuilCentrality >= 2) {
                print("Please enter a number inferior to 2 (which is the maximum possible value).")
            }
            else {
                answered = TRUE
            }
        }
    }
    vertexAttributes = matrix(nrow = 0, ncol = 2, dimnames = list(NULL, 
        c("label", "color")))
    for (i in 1:nrow(reseau$conflictsTotal)) {
        if (reseau$conflictsTotal[i, 2] > seuilCentrality) {
            vertexAttributes = rbind(vertexAttributes, matrix(data = c("overconflicting", 
                "red"), nrow = 1, ncol = 2, dimnames = list(rownames(reseau$conflictsTotal)[i], 
                NULL)))
        }
        else {
            if (reseau$conflictsTotal[i, 2] > 0) {
                vertexAttributes = rbind(vertexAttributes, matrix(data = c("unknown", 
                  "grey"), nrow = 1, ncol = 2, dimnames = list(rownames(reseau$conflictsTotal)[i], 
                  NULL)))
            }
        }
    }
    adjacencyTable = as.matrix.network(myNetwork)
    for (i in 1:nrow(vertexAttributes)) {
        if (vertexAttributes[i, 1] != "overconflicting") {
            undecidable = 0
            for (j in 1:ncol(adjacencyTable)) {
                if (adjacencyTable[rownames(vertexAttributes)[i], 
                  j] > 0) {
                  if (vertexAttributes[colnames(adjacencyTable)[j], 
                    1] != "overconflicting") {
                    undecidable = undecidable + 1
                  }
                }
            }
            if (undecidable == 0) {
                vertexAttributes[i, ] = c("sober", "green")
            }
        }
    }
    vertexNameId = network.vertex.names(myNetwork)
    vertexId = NULL
    for (i in 1:nrow(vertexAttributes)) {
        vertexId = c(vertexId, which(vertexNameId == rownames(vertexAttributes)[i]))
    }
    myNetwork = set.vertex.attribute(myNetwork, "color", vertexAttributes[, 
        "color"], v = vertexId)
    gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork), 
        gmode = "graph", vertex.col = get.vertex.attribute(myNetwork, 
            "color"), boxed.labels = TRUE)
    par(ask = FALSE)
    reseau$vertexAttributes = vertexAttributes
    return(reseau)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

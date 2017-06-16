\name{PCC.elimination}
\alias{PCC.elimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
~~ PCC.elimination is dedicated to the elimination of problematic variant locations, as defined in Poole (1974) and Camps & Cafiero (2015). It helps the user defining a threshold above which variant locations are considered to be over-conflicting. It returns a table where those variant locations are labeled as problematic. ~~
}
\usage{
PCC.elimination(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
~~ \code{x}: a PCC.conflict object ~~
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
 ~ Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1)
 Eric Poole, « The Computer in Determining Stemmatic Relationships », Computers and the Humanities 8-4, 1974, p. 207‑16
 
 ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
~~ To remove the variants labeled as problematic: \code{\link{PCC.doElimination}} About the input for this fonction : \code{\link{PCC.conflicts}

~~~
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

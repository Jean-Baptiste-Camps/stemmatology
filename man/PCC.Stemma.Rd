\name{PCC.Stemma}
\alias{PCC.Stemma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.Stemma(x, omissionsAsReadings = FALSE, limit = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{omissionsAsReadings}{
%%     ~~Describe \code{omissionsAsReadings} here~~
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
function (x, omissionsAsReadings = FALSE, limit = 0) 
{
    tableVariantes = x
    edgelistGlobal = matrix(c(character(0), character(0)), ncol = 2)
    modelsGlobal = as.list(NULL)
    modelsByGroupGlobal = as.list(NULL)
    counter = 0
    while (ncol(tableVariantes) > 3) {
        counter = counter + 1
        pccDesagreement = PCC.desagreement(tableVariantes)
        pccBuildGroup = PCC.buildGroup(pccDesagreement)
        if (identical(pccBuildGroup$groups, list())) {
            stop("No group was found. Unable to build stemma.")
        }
        pccReconstructModel = PCC.reconstructModel(pccBuildGroup)
        tableVariantes = pccReconstructModel$database
        if (!exists("tableVariantes")) {
            print("Voil\303\240 qui est dr\303\264lement bizarre.")
            return(tableVariantes)
        }
        edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
        modelsGlobal[[counter]] = pccReconstructModel$models
        modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
    }
    stemma = as.network(edgelistGlobal, directed = TRUE, matrix.type = "edgelist")
    gplot(stemma, displaylabels, label = network.vertex.names(stemma), 
        gmode = "digraph", boxed.labels = TRUE, usearrows = TRUE)
    if (is.null(tableVariantes)) {
        output = as.list(NULL)
        output$edgelist = edgelistGlobal
        output$database = tableVariantes
        output$modelsGlobal = modelsGlobal
        output$modelsByGroupGlobal = modelsByGroupGlobal
        return(output)
    }
    if (ncol(tableVariantes) > 1) {
        writeLines("There is now less than four manuscripts in the database. Stemma building has now lost in accuracy. Do you want to continue anyway (take last step with caution) ? Y/N\n")
        answered = FALSE
        while (answered == FALSE) {
            answer = readline("(Y/N)")
            if (answer != "N" && answer != "Y") {
                print("Please enter Y (yes) or N (no).")
            }
            if (answer == "N") {
                output = as.list(NULL)
                output$edgelist = edgelistGlobal
                output$database = tableVariantes
                output$modelsGlobal = modelsGlobal
                output$modelsByGroupGlobal = modelsByGroupGlobal
                return(output)
            }
            if (answer == "Y") {
                counter = counter + 1
                pccDesagreement = PCC.desagreement(tableVariantes)
                pccBuildGroup = PCC.buildGroup(pccDesagreement)
                pccReconstructModel = PCC.reconstructModel(pccBuildGroup)
                tableVariantes = pccReconstructModel$database
                edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
                modelsGlobal[[counter]] = pccReconstructModel$models
                modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
                stemma = as.network(edgelistGlobal, directed = TRUE, 
                  matrix.type = "edgelist")
                gplot(stemma, displaylabels, label = network.vertex.names(stemma), 
                  gmode = "digraph", boxed.labels = TRUE, usearrows = TRUE)
                output = as.list(NULL)
                output$edgelist = edgelistGlobal
                output$database = tableVariantes
                output$modelsGlobal = modelsGlobal
                output$modelsByGroupGlobal = modelsByGroupGlobal
                return(output)
            }
        }
    }
    else {
        output = as.list(NULL)
        output$edgelist = edgelistGlobal
        output$database = tableVariantes
        output$modelsGlobal = modelsGlobal
        output$modelsByGroupGlobal = modelsByGroupGlobal
        return(output)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

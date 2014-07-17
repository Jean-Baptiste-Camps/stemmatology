\name{PCC.reconstructModel}
\alias{PCC.reconstructModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.reconstructModel(x, omissionsAsReadings = FALSE)
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
    groups = x$groups
    tableVariantes = x$database
    newDatabase = x$database
    modelsReconstructed = as.list(NULL)
    models = as.list(NULL)
    edgelist = matrix(c(character(0), character(0)), ncol = 2)
    groupsLabels = NULL
    descripti = NULL
    for (g in 1:length(groups)) {
        thisGroupLabels = paste(groups[[g]], collapse = "")
        groupsLabels = c(groupsLabels, thisGroupLabels)
    }
    myDimnames = as.list(NULL)
    modelsByGroup = matrix(nrow = 1, ncol = length(groups), dimnames = list("Models", 
        groupsLabels))
    for (i in 1:length(groups)) {
        myGroup = as.vector(groups[[i]])
        labelMyMss = paste(myGroup, collapse = "")
        print(paste("Now comparing group", labelMyMss))
        labelMyModel = paste("x(", labelMyMss, ")", sep = "")
        myModel = matrix(nrow = nrow(tableVariantes), ncol = 1, 
            dimnames = c(labels(tableVariantes)[1], labelMyModel))
        for (j in 1:nrow(tableVariantes)) {
            if (length(levels(as.factor(tableVariantes[j, myGroup]))) == 
                1) {
                myModel[j, ] = as.integer(levels(as.factor(tableVariantes[j, 
                  myGroup])))
            }
            if (length(levels(as.factor(tableVariantes[j, myGroup]))) > 
                1) {
                myReadings = as.numeric(levels(as.factor(tableVariantes[j, 
                  myGroup])))
                myVL = tableVariantes[j, ]
                myCommonReading = NULL
                for (l in 1:length(myReadings)) {
                  if (omissionsAsReadings == FALSE) {
                    if (length(myVL[myVL == myReadings[l] & myVL != 
                      0 & !is.na(myVL)]) > 1) {
                      myCommonReading = c(myCommonReading, myReadings[l])
                    }
                  }
                  if (omissionsAsReadings == TRUE) {
                    if (length(myVL[myVL == myReadings[l] & !is.na(myVL)]) > 
                      1) {
                      myCommonReading = c(myCommonReading, myReadings[l])
                    }
                  }
                }
                if (length(myCommonReading) > 1) {
                  stop(paste("More than one non-singular reading for the group ", 
                    labelMyMss, " at VL ", rownames(tableVariantes)[j], 
                    ". This is normally not possible and looks like a bug or inconsistency. Try setting omissionsAsReadings to FALSE.", 
                    sep = ""))
                }
                if (length(myCommonReading) == 1) {
                  myModel[j, ] = myCommonReading
                }
                if (length(myCommonReading) == 0) {
                  print(paste("Reading not assessable for the group ", 
                    labelMyMss, " at VL ", rownames(tableVariantes)[j], 
                    ". This can happen sometimes.", sep = ""))
                }
            }
        }
        extantModel = NULL
        myGroupAndModel = cbind(tableVariantes[, myGroup], myModel)
        myGroupComp = PCC.desagreement(myGroupAndModel)
        for (m in 1:length(myGroup)) {
            if (myGroupComp$severeDesagreement[myGroup[m], labelMyModel] == 
                0 && myGroupComp$benigneDesagreement[myGroup[m], 
                labelMyModel] == 0 && (myGroupComp$omissionsOriented[myGroup[m], 
                labelMyModel] == 0 | is.na(myGroupComp$omissionsOriented[myGroup[m], 
                labelMyModel]))) {
                print(paste(myGroup[m], "seems to be the model of group", 
                  labelMyMss))
                extantModel = c(extantModel, myGroup[m])
            }
            else {
                print(paste(myGroup[m], "has", myGroupComp$severeDesagreement[myGroup[m], 
                  labelMyModel], "severe desagreement(s),", myGroupComp$benigneDesagreement[myGroup[m], 
                  labelMyModel], "benigne desagreement(s),", 
                  myGroupComp$omissionsOriented[myGroup[m], labelMyModel], 
                  "omissions", "towards the virtual model. It does not seem to be the model"))
            }
        }
        keepVirtualModel = FALSE
        if (length(extantModel) > 1) {
            theModels = paste(extantModel, collapse = " ")
            message = paste("More than one manuscript (", theModels, 
                ") can be identified to the model of group", 
                labelMyMss, ".\n There is not enough data to decide (and it is recommended to increase database size/number of observations if possible).\n Do you whish to chose manually which manuscript is the model (C),\n or to keep the reconstructed virtual model as the model (K) \n or quit (Q) ?\n", 
                collapse = "")
            writeLines(message)
            answered = FALSE
            while (answered == FALSE) {
                answerOne = readline("(C/K/Q)")
                if (answerOne != "C" && answerOne != "K" && answerOne != 
                  "Q") {
                  print("Please enter C (choose), K (Keep) or Q (Quit).")
                }
                if (answerOne == "Q") {
                  return()
                }
                if (answerOne == "K") {
                  colnames(modelsByGroup)[i] = labelMyMss
                  modelsByGroup[, i] = labelMyModel
                  models[[i]] = as.matrix(myModel)
                  answered = TRUE
                  keepVirtualModel = TRUE
                }
                if (answerOne == "C") {
                  chosen = FALSE
                  while (chosen == FALSE) {
                    message = paste("You may choose one between", 
                      theModels, ". Which one do you want?\n")
                    answerTwo = readline(message)
                    if (!answerTwo \%in\% extantModel) {
                      print(paste("Oops, you must choose one of", 
                        theModels))
                    }
                    if (answerTwo \%in\% extantModel) {
                      extantModel = answerTwo
                      chosen = TRUE
                      answered = TRUE
                    }
                  }
                }
            }
        }
        if (length(extantModel) == 1 && keepVirtualModel == FALSE) {
            print(paste(extantModel, "is the only ms. inside the group that seems to be the model of group", 
                labelMyMss))
            colnames(modelsByGroup)[i] = labelMyMss
            modelsByGroup[, i] = extantModel
        }
        if (length(extantModel) == 0 && keepVirtualModel == FALSE) {
            writeLines(paste("No ms inside group", labelMyMss, 
                "seems to be the model. We will proceed\n to a comparison with mss outside the group."))
            others = colnames(tableVariantes)[!colnames(tableVariantes) \%in\% 
                myGroup]
            othersAndModel = cbind(tableVariantes[, others, drop = FALSE], 
                myModel)
            myOthersComp = PCC.desagreement(othersAndModel)
            for (n in 1:length(others)) {
                if (myOthersComp$severeDesagreement[others[n], 
                  labelMyModel] == 0 && myOthersComp$benigneDesagreement[others[n], 
                  labelMyModel] == 0 && (myOthersComp$omissionsOriented[others[n], 
                  labelMyModel] == 0 | is.na(myOthersComp$omissionsOriented[others[n], 
                  labelMyModel]))) {
                  print(paste(others[n], "seems to be the model."))
                  extantModel = c(extantModel, others[n])
                }
                else {
                  print(paste(others[n], "has", myOthersComp$severeDesagreement[others[n], 
                    labelMyModel], "severe desagreement(s),", 
                    myOthersComp$benigneDesagreement[others[n], 
                      labelMyModel], "benigne desagreement(s),", 
                    myOthersComp$omissionsOriented[others[n], 
                      labelMyModel], "omissions", "towards the virtual model. It does not seem to be the model"))
                }
            }
            if (length(extantModel) > 1) {
                extantModel = paste(extantModel, collapse = "")
                stop(paste("It appears that more than one manuscript (", 
                  extantModel, ") can be the model of group", 
                  labelMyMss, ". There is not enough data to decide. Stemma building will stop NOW.", 
                  collapse = ""))
            }
            if (length(extantModel) == 1) {
                print(paste(extantModel, "seems to be the model of this group"))
                colnames(modelsByGroup)[i] = labelMyMss
                modelsByGroup[, i] = extantModel
            }
            if (length(extantModel) == 0) {
                writeLines(paste("No extant ms. at all seems to be the model of the group", 
                  labelMyMss, ".\n It is presumably a lost ms."))
                colnames(modelsByGroup)[i] = labelMyMss
                modelsByGroup[, i] = labelMyModel
                models[[i]] = as.matrix(myModel)
            }
        }
        for (p in 1:length(myGroup)) {
            if (myGroup[p] != modelsByGroup[i]) {
                edgelist = rbind(edgelist, c(modelsByGroup[i], 
                  myGroup[p]))
                descripti = c(descripti, myGroup[p])
            }
        }
        modelsReconstructed[[i]] = myModel
    }
    if (length(models) > 0) {
        for (x in 1:length(models)) {
            tableVariantes = cbind(tableVariantes, models[[x]])
        }
    }
    output = as.list(NULL)
    output$oldDatabase = tableVariantes
    nonDescripti = colnames(tableVariantes)[!colnames(tableVariantes) \%in\% 
        descripti]
    if (identical(nonDescripti, character(0))) {
        print(descripti)
        print(colnames(tableVariantes))
        print(nonDescripti)
        print("The stemma is complete")
        database = tableVariantes[, nonDescripti, drop = FALSE]
    }
    else {
        database = tableVariantes[, nonDescripti, drop = FALSE]
    }
    output$database = database
    stemma = as.network(edgelist, directed = TRUE, matrix.type = "edgelist")
    gplot(stemma, displaylabels, label = network.vertex.names(stemma), 
        gmode = "digraph", boxed.labels = TRUE, usearrows = TRUE)
    output$edgelist = edgelist
    output$models = modelsReconstructed
    output$modelsByGroup = modelsByGroup
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

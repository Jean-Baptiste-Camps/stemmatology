PCC.reconstructModel <-
function(x, omissionsAsReadings = FALSE, recoverNAs = TRUE) {
    # Ajouter la conservation de la base de données initiales, dans sa version complétée
    # Is omissionsAsReadings implemented ?
    # Function to reconstruct the model for a group of ms.  Take in entry an
    # object of type PCC.buildGroups, that is a list of vectors of mss labels
    # (one for each group) Gives back the vector containing the virtual model
    # values or the identification of the model, and the links between the
    # mss in the group in the form of an edgelist TODO(JBC): add an option to
    # include in the output the edgelength, computed as = to the number of
    # disagreement between a ms. and his model.
    # Added option recoverNAs (false by default) which is an update to the method. If used, when actual or virtual manuscripts are identified to a reconstructed models, every NA they have is «recovered» by taking the value of the model.
    groups = x$groups
    tableVariantes = x$database
    newDatabase = x$database
    modelsReconstructed = as.list(NULL)
    models = as.list(NULL)  # The edgelist that will contain the stemmatic information
    # edgelist = c(character(0),character(0));
    edgelist = matrix(c(character(0), character(0)), ncol = 2)  # We create a matrix of models for each group, but we have to create the labels first
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
        # For each variantlocation
        myGroup = as.vector(groups[[i]])  # For the moment, the label of the virtual model is of the form x(myMss)
        # It could perhaps be replaced by a random alphanumeric of length 2 or 3?
        labelMyMss = paste(myGroup, collapse = "")  ##Debug:
        print(paste("Now comparing group", labelMyMss))  ##
        labelMyModel = paste("x(", labelMyMss, ")", sep = "")
        myModel = matrix(nrow = nrow(tableVariantes), ncol = 1, dimnames = c(labels(tableVariantes)[1], 
            labelMyModel))
        for (j in 1:nrow(tableVariantes)) {
            # We compare the readings of the differents manuscripts of the group. if
            # length = 0, all is na, and the value for the model stays NA if length =
            # 1, only one reading, attribuable au modèle
            if (length(levels(as.factor(tableVariantes[j, myGroup]))) == 
                1) {
                myModel[j, ] = as.integer(levels(as.factor(tableVariantes[j, 
                  myGroup])))
            }
            # if length > 1, alors il faut trancher
            if (length(levels(as.factor(tableVariantes[j, myGroup]))) > 1) {
                # First thing to do is to see if some of the reading are singular
                # readings or omissions
                myReadings = as.numeric(levels(as.factor(tableVariantes[j, 
                  myGroup])))
                myVL = tableVariantes[j, ]  # We create a vector containing non-singular reading
                myCommonReading = NULL
                for (l in 1:length(myReadings)) {
                  # We select readings that happens more than once in the whole tradition
                  if (omissionsAsReadings == FALSE) {
                    # Normal case, omissions are not considered as common readings
                    if (length(myVL[myVL == myReadings[l] & myVL != 0 & !is.na(myVL)]) > 1) {
                      myCommonReading = c(myCommonReading, myReadings[l])
                    }
                  }
                  if (omissionsAsReadings == TRUE) {
                    # Option : omission are considered readings
                    if (length(myVL[myVL == myReadings[l] & !is.na(myVL)]) > 
                      1) {
                      myCommonReading = c(myCommonReading, myReadings[l])
                    }
                  }
                }
                if (length(myCommonReading) > 1) {
                  stop(paste("More than one non-singular reading for the group ", 
                    labelMyMss, " at VL ", rownames(tableVariantes)[j], ". This is normally not possible and looks like a bug or inconsistency. Try setting omissionsAsReadings to FALSE.",  tableVariantes[j, myGroup],
                    sep = ""))
                }
                if (length(myCommonReading) == 1) {
                  # Only one common reading, that is affected to the model
                  myModel[j, ] = myCommonReading
                }
                if (length(myCommonReading) == 0) {
                  # If there is no common reading, the reading of the model remains
                  # unassessable (NA)
                  print(paste("Reading not assessable for the group ", labelMyMss, 
                    " at VL ", rownames(tableVariantes)[j], ". This can happen sometimes.", 
                    sep = ""))
                }
            }
        }
        ##### STEP 2, compare mss to the model Now we have reconstructed the virtual
        ##### model for the group. We must proceed to compare it to: 1. all extant
        ##### mss in the group 2. all extant mss outside the group And see if there
        ##### is a match. If not, the virtual model is supposed to be a lost ms.
        extantModel = NULL  # We bind the mss from the group with the virtual model
        myGroupAndModel = cbind(tableVariantes[, myGroup], myModel)  # We compare them ####TODO(JBC): it might not be a good idea to have this level 1 function call another level 1 function. Perhaps the comparison should go in the higher level global function...
        myGroupComp = PCC.disagreement(myGroupAndModel)
        for (m in 1:length(myGroup)) {
            if (myGroupComp$severeDisagreement[myGroup[m], labelMyModel] == 
                0 && myGroupComp$benigneDisagreement[myGroup[m], labelMyModel] == 
                0 && (myGroupComp$omissionsOriented[myGroup[m], labelMyModel] == 
                0 | is.na(myGroupComp$omissionsOriented[myGroup[m], labelMyModel]))) {
                print(paste(myGroup[m], "seems to be the model of group", 
                  labelMyMss))
                extantModel = c(extantModel, myGroup[m])
            } else {
                print(paste(myGroup[m], "has", myGroupComp$severeDisagreement[myGroup[m], 
                  labelMyModel], "severe disagreement(s),", myGroupComp$benigneDisagreement[myGroup[m], 
                  labelMyModel], "benigne disagreement(s),", myGroupComp$omissionsOriented[myGroup[m], 
                  labelMyModel], "omissions", "towards the virtual model. It does not seem to be the model"))
            }
        }
        # Now that the comparison is done, we verify that the results are
        # consistent, and proceed to affect the role of model to the good ms.
        # DEBUG: print(extantModel);
        keepVirtualModel = FALSE
        if (length(extantModel) > 1) {
            theModels = paste(extantModel, collapse = " ")
            message = paste("More than one manuscript (", theModels, ") can be identified to the model of group", 
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
                  modelsByGroup[, i] = labelMyModel  # models[[i]] = myModel #This seems to create a weird bug (again with R and its object classes...). I'll try this for the moment :
                  models[[i]] = as.matrix(myModel)
                  answered = TRUE
                  keepVirtualModel = TRUE
                }
                if (answerOne == "C") {
                  chosen = FALSE
                  while (chosen == FALSE) {
                    message = paste("You may choose one between", theModels, 
                      ". Which one do you want?\n")
                    answerTwo = readline(message)
                    if (!answerTwo %in% extantModel) {
                      print(paste("Oops, you must choose one of", theModels))
                    }
                    if (answerTwo %in% extantModel) {
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
            # option to recover NAs when a ms. of the database is identified with a just reconstructed model
            #first we test if the option is activated and NA's actually exist in the extant model
            if (recoverNAs == TRUE && (length(which(is.na(tableVariantes[, extantModel]))) > 0)) {
                for (r in 1:nrow(tableVariantes)) {
                    # for each reading, if it is NA in the ms. but not in the reconstructed model, we replace it.
                    if ((is.na(tableVariantes[r, extantModel])) && !is.na(myModel[r,]) ) {
                        # Debug:
                        #readline(paste("Recovering reading of virtual model", labelMyModel, "at VL", r, "Value was", tableVariantes[r, extantModel], "will be", myModel[r,]))
                        tableVariantes[r, extantModel] = myModel[r,]
                        #debug: readline(paste("value is now", tableVariantes[r, extantModel]))
                    }
                }
            }
        }
        if (length(extantModel) == 0 && keepVirtualModel == FALSE) {
            # No ms. inside this group corresponds. We proceed to compare with mss
            # outside the group. NB & TODO(GLOBAL): this step (that we included in
            # the paper) is PROBABLY not necessary, nor algorithmically consistent.
            # We NEED to think about it. How can the model be outside the group and
            # have no disagreement with the model, knowing that the virtual model is
            # reconstructed based on common readings to the mss of the group, and
            # that these are, at least once, unique to this group? Yet the complexity
            # of this principle is very high, and intuition hard, so we need to
            # THINK.
            writeLines(paste("No ms inside group", labelMyMss, "seems to be the model. We will proceed\n to a comparison with mss outside the group."))  #NB: si nous voulons être rigoureux, il faut que la base de données inclue également les mss retirés aux étapes précédentes?
            # We create a database containing the model and the mss outside the group
            others = colnames(tableVariantes)[!colnames(tableVariantes) %in% 
                myGroup]
            # if there are still other manuscripts
            if (length(others) > 0) {
                othersAndModel = cbind(tableVariantes[, others, drop = FALSE], 
                myModel)  # We compare them ####TODO(JBC): it might not be a good idea to have this level 1 function call another level 1 function. Perhaps the comparison should go in the higher level global function...
            myOthersComp = PCC.disagreement(othersAndModel)
            for (n in 1:length(others)) {
                if (myOthersComp$severeDisagreement[others[n], labelMyModel] == 
                  0 && myOthersComp$benigneDisagreement[others[n], labelMyModel] == 
                  0 && (myOthersComp$omissionsOriented[others[n], labelMyModel] == 
                  0 | is.na(myOthersComp$omissionsOriented[others[n], labelMyModel]))) {
                  print(paste(others[n], "seems to be the model."))
                  extantModel = c(extantModel, others[n])
                } else {
                  print(paste(others[n], "has", myOthersComp$severeDisagreement[others[n], 
                    labelMyModel], "severe disagreement(s),", myOthersComp$benigneDisagreement[others[n], 
                    labelMyModel], "benigne disagreement(s),", myOthersComp$omissionsOriented[others[n], 
                    labelMyModel], "omissions", "towards the virtual model. It does not seem to be the model"))
                }
            }
            # Now that the comparison is done, we verify that the results are
            # consistent, and proceed to affect the role of model to the good ms.
            if (length(extantModel) > 1) {
                extantModel = paste(extantModel, collapse = "")
                stop(paste("It appears that more than one manuscript (", 
                  extantModel, ") can be the model of group", labelMyMss, 
                  ". There is not enough data to decide. Stemma building will stop NOW.", 
                  collapse = ""))
            }
            if (length(extantModel) == 1) {
                print(paste(extantModel, "seems to be the model of this group"))
                colnames(modelsByGroup)[i] = labelMyMss
                modelsByGroup[, i] = extantModel
                # option to recover NAs when a ms. of the database is identified with a just reconstructed model
                #first we test if the option is activated and NA's actually exist in the extant model
                if (recoverNAs == TRUE && (length(which(is.na(tableVariantes[, extantModel]))) > 0)) {
                    for (r in 1:nrow(tableVariantes)) {
                        # for each reading, if it is NA in the ms. but not in the reconstructed model, we replace it.
                        if ((is.na(tableVariantes[r, extantModel])) && !is.na(myModel[r,]) ) {
                            tableVariantes[r, extantModel] = myModel[r,]
                        }
                    }
                }
            }
            } else {
                print("There are no other manuscript left in the database.")
            }
            
            if (length(extantModel) == 0) {
                # If length is STILL equal to 0, then the manuscript is lost, and we keep
                # the virtual model
                writeLines(paste("No extant ms. at all seems to be the model of the group", 
                  labelMyMss, ".\n It is presumably a lost ms."))
                colnames(modelsByGroup)[i] = labelMyMss
                modelsByGroup[, i] = labelMyModel  # models[[i]] = myModel #This seems to create a weird bug (again with R and its object classes...). I'll try this for the moment :
                models[[i]] = as.matrix(myModel)  #DEBUG :
                # print('CAREFUL:') print(as.matrix(models[[i]]))
                # write.table(models[[i]], file =
                # '/home/jbc/Data/F/Stemmatologie/R/temp')
            }
        }
        ### Here the function to create the edgelist If we want to modify edge
        ### length, it might be possible using the phylo package...  See :
        ### [R-sig-phylo] convert edge list to phylo object
        ### https://stat.ethz.ch/pipermail/r-sig-phylo/2009-July/000404.html for
        ### each manuscript in the group
        for (p in 1:length(myGroup)) {
            # if he is not the model
            if (myGroup[p] != modelsByGroup[i]) {
                # we add a link between the model and him in the edgelist
                edgelist = rbind(edgelist, c(modelsByGroup[i], myGroup[p]))  # we add the manuscript to the descripti (to be removed) list
                descripti = c(descripti, myGroup[p])
            }
        }
        modelsReconstructed[[i]] = myModel
    }
    # models is the list containing the database for each virtual model, if
    # and only if the virtual model could not be identified with an existing
    # one. In that case, we need to add this virual model to the database for
    # further calculation.
    if (length(models) > 0) {
        for (x in 1:length(models)) {
            tableVariantes = cbind(tableVariantes, models[[x]])
        }
    }
    # Preparing the output
    output = as.list(NULL)
    output$oldDatabase = tableVariantes  # the new database
    nonDescripti = colnames(tableVariantes)[!colnames(tableVariantes) %in% 
        descripti]  # We need to test if there are only descripti
    if (identical(nonDescripti, character(0))) {
        # debug:
        print(descripti)
        print(colnames(tableVariantes))
        print(nonDescripti)
        print("The stemma is complete")
        database = tableVariantes[, nonDescripti, drop = FALSE]
    } else {
        # print('Was ist diese Scheisse?'); print(models); print(database);
        # writeLines('I am the Column With No Name \n Fear me, For I have No
        # Name \n I am your worst nightmare'); print(nonDescripti);
        # print(descripti) # OK, this costed me half a night sleep : when you
        # select from an object, R (bloody him), coerces it to the lowest
        # possible form by default. That is, when you select one (and only one)
        # column from a matrix, it suddenly becomes a vector without colname...
        # So, to avoid that, you have to set ,drop = FALSE TODO(JBC): VERY
        # IMPORTANT THAT WE DO THAT ELSEWHERE IN THE PROGRAM
        database = tableVariantes[, nonDescripti, drop = FALSE]
    }
    output$database = database  # the edgelist
    # Debug: plot the stemma (no edge length modification for the moment)
    stemma = as.network(edgelist, directed = TRUE, matrix.type = "edgelist")
    gplot(stemma, displaylabels, label = network.vertex.names(stemma), gmode = "digraph", 
        boxed.labels = TRUE, usearrows = TRUE)
    output$edgelist = edgelist  # and the rest
    output$models = modelsReconstructed
    output$modelsByGroup = modelsByGroup
    return(output)  ## Adjust edgelength using igraph => NB, incompatible avec network, donc fait buguer les autres fonctions...
    # library(igraph) edgelist = matrix(
    # c('A','B','B','A','A','B','C','D','F','G'), ncol = 2 ) g =
    # graph(edgelist, directed = TRUE) Pour calculer le poids, qui est
    # inversement proportionnel à la distance (désaccords+omissionsoreientées
    # dans les deux sens), en le rendant égal à 1/d length = c(29, 12, 10, 0,
    # 28) ## On a besoin que le poids soit une valeur positive pour que ça
    # marche à peu près, et plus le poids est élevé, plus on va avoir des
    # nœuds proches. Pour l'implémenter, il faudrait donc calculer le nombre
    # maximal de désaccords dans toute la tradition, mettons par ex. 30, et
    # soustraire pour chaque ms. son nombre de désaccord de ce total, par ex.
    # pour un ms. ayant deux désaccords, on passe à poids = 28, etc. et pour
    # celui en ayant 30 à 0 (la solution d'utiliser des poids négatifs fait
    # buguer l'algorithme) E(g)$weight = length l =
    # layout.fruchterman.reingold(g, weights=E(g)$weight, niter = 100000 )
    # plot(g, layout=l) Autre solution, plus dans l'esprit (mais ne gérant
    # pas la contamination?), utiliser le module phylo. À partir d'une
    # edgelist :
    # https://stat.ethz.ch/pipermail/r-sig-phylo/2009-July/000405.html
}

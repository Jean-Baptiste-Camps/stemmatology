PCC.reconstructModel <-
  function(x, omissionsAsReadings = FALSE, recoverNAs = TRUE, ask = TRUE, verbose = FALSE) {
    # Function to reconstruct the model for a group of witnesses.
    # Take in entry an
    # object of type PCC.buildGroups, with a list of vectors of wit. labels
    # (one for each group).
    # Gives back the vector containing the virtual model
    # values or the identification of the model, and the links between the
    # wit. in the group in the form of an edgelist
    # TODO(JBC): add an option to
    # include in the output the edgelength, computed as = to the number of
    # disagreement between a ms. and his model.
    # Added option recoverNAs (TRUE by default) which is an update to the method.
    # If used, when actual or virtual manuscripts are identified to a
    # reconstructed models, every NA they have is «recovered» by taking
    # the value of the model.
    # TODO: do we want to implement a:
    # \item{limit}{
    #   The maximum number of disagreements allowed to identify a wit.
    #  with the reconstructed model.
    #  Default: \code{0}.
    #  } ?
    groups = x$groups
    tableVariantes = x$database
    newDatabase = x$database
    modelsReconstructed = matrix(
      nrow = nrow(tableVariantes), 
      ncol = 0, 
      dimnames = list(dimnames(tableVariantes)[[1]]))
    # modelsToAdd is the list that will contain
    # only the reconstructed models that should be
    # added to the database.
    modelsToAdd = as.list(NULL)  
    # The edgelist that will contain the stemmatic information
    edgelist = matrix(c(character(0), character(0), character(0)), ncol = 3)
    # We create a matrix of models for each group, but we have
    # to create the labels first
    groupsLabels = NULL
    descripti = NULL
    #Create a vector with the common label of each group
    for (g in seq_len(length(groups))) {
      groupsLabels = c(groupsLabels, paste(groups[[g]], collapse = ""))
    }
    #Create a matrix with the labels of the models for each group
    modelsByGroup = matrix(
      nrow = 1,
      ncol = length(groups),
      dimnames = list("Models",groupsLabels)
    )
    for (i in 1:length(groups)) {
      # For each variantlocation
      myGroup = as.vector(groups[[i]])
      # The label of the virtual model could perhaps 
      # be replaced by a random alphanumeric of length 2 or 3?
      labelMyMss = paste(myGroup, collapse = "")  
      ##Debug:
      if (verbose) {
        cat("Now comparing group", labelMyMss,"\n")
      }
      labelMyModel = paste("{", labelMyMss, "}", sep = "")
      myModel = matrix(
        nrow = nrow(tableVariantes),
        ncol = 1,
        dimnames = c(labels(tableVariantes)[1],
                     labelMyModel)
      )
      for (j in seq_len(nrow(tableVariantes))) {
        # We compare the readings of the differents manuscripts of the group. if
        # length = 0, all is na, and the value for the model stays NA if length =
        # 1, only one reading, attributed to the model
        if #(length(levels(as.factor(tableVariantes[j, myGroup]))) ==
        (length(
          unique(
            tableVariantes[j, myGroup][!is.na(tableVariantes[j, myGroup])]
            )
          ) == #a LOT faster
            1) {
          myModel[j,] = unique(tableVariantes[j,myGroup][!is.na(tableVariantes[j, myGroup])])
        }
        # if length > 1, alors il faut trancher
        if (
          length(
            unique(
              tableVariantes[j, myGroup][!is.na(tableVariantes[j, myGroup])]
              )
            ) > 1) {
          # First thing to do is to see if some of the reading are singular
          # readings or omissions
          # The readings of my group:
          myReadings = unique(tableVariantes[j, myGroup][!is.na(tableVariantes[j, myGroup])])
          # The full VL
          myVL = tableVariantes[j,]  # We create a vector containing non-singular reading
          # Placeholder for readings shared with mss outside the group
          myCommonReading = NULL
          for (l in seq_len(length(myReadings))) {
            # We select readings that happens more than once in the whole tradition
            if (omissionsAsReadings == FALSE) {
              # Normal case, omissions are not considered as common readings
              if (length(myVL[myVL == myReadings[l] &
                              myVL != 0 & !is.na(myVL)]) > 1) {
                myCommonReading = c(myCommonReading, myReadings[l])
              }
            }
            if (omissionsAsReadings == TRUE) {
              # Option : omission are considered readings
              if (length(myVL[myVL == myReadings[l] &
                              !is.na(myVL)]) >
                  1) {
                myCommonReading = c(myCommonReading, myReadings[l])
              }
            }
          }
          if (length(myCommonReading) > 1) {
            stop(
              paste(
                "More than one non-singular reading for the group ",
                labelMyMss,
                " at VL ",
                rownames(tableVariantes)[j],
                ".\nThis is normally not possible and looks like a bug or inconsistency.", 
                "\nTry setting omissionsAsReadings to FALSE.",
                sep = ""
              )
            )
          }
          if (length(myCommonReading) == 1) {
            # Only one common reading, that is affected to the model
            myModel[j,] = myCommonReading
          }
          if (length(myCommonReading) == 0) {
            # If there is no common reading, the reading of the model remains
            # unassessable (NA)
            if (verbose) {
              cat(
                "Reading not assessable for the group",
                labelMyMss,
                "at VL",
                rownames(tableVariantes)[j],
                "\n"
                #,
                #"\nThis can happen sometimes."
                )
            }
          }
        }
      }
      ##### STEP 2, compare witnesses to the model.
      ##### Now we have reconstructed the virtual
      ##### model for the group. We must proceed to compare it to: 
      ##### 1. all extant wit. in the group 
      ##### 2. all extant wit. outside the group 
      ##### And see if there is a match. 
      ##### NB (step 2 probably not necessary in most cases, because of 
      ##### severe disagreements).
      ##### If not, the virtual model is supposed to be a lost ms.
      extantModel = NULL
      # We bind the mss from the group with the virtual model
      myGroupAndModel = cbind(tableVariantes[, myGroup], myModel)  
      # We compare them 
      myGroupComp = PCC.disagreement(myGroupAndModel)
      for (m in seq_len(length(myGroup))) {
        #TODO: only 0 or add an option to treat NA as zeros? (deal 
        # with it in PCC.disagreement)
        if (myGroupComp$severeDisagreement[myGroup[m], labelMyModel] ==
            0 &&
            myGroupComp$benignDisagreement[myGroup[m], labelMyModel] ==
            0 &&
            (myGroupComp$omissionsOriented[myGroup[m], labelMyModel] ==
             0 |
             is.na(myGroupComp$omissionsOriented[myGroup[m], labelMyModel]))) {
          if (verbose) {
            cat(myGroup[m], "seems to be the model of group",
                        labelMyMss, "\n")
          }
          extantModel = c(extantModel, myGroup[m])
        } else {
          if (verbose) {
            cat(
              myGroup[m],
              "has\n",
              myGroupComp$severeDisagreement[myGroup[m],
                                             labelMyModel],
              "severe disagreement(s),\n",
              myGroupComp$benignDisagreement[myGroup[m],
                                              labelMyModel],
              "benign disagreement(s),\n",
              myGroupComp$omissionsOriented[myGroup[m],
                                            labelMyModel],
              "omissions",
              "\ntowards the virtual model.\nIt does not seem to be the model\n"
            )
          }
        }
      }
      # Now that the comparison is done, we verify that the results are
      # consistent, and proceed to affect the role of model to the good ms.
      # DEBUG: print(extantModel);
      keepVirtualModel = FALSE
      if (length(extantModel) > 1) {
        if(ask){
          theModels = paste(extantModel, collapse = " ")
          message = paste(
            "More than one manuscript (",
            theModels,
            ") can be identified to the model of group ",
            labelMyMss,
            ".\nThere is not enough data to decide (increase database if possible).",
            "\nDo you wish to: \n\tchose manually which manuscript is the model (C),",
            "\n\tor to keep the reconstructed virtual model as the model (K)",
            "\n\tor quit (Q) ?\n",
            sep = ""
          )
          writeLines(message)
          answered = FALSE
          while (answered == FALSE) {
            answerOne = readline("(C/K/Q)")
            if (answerOne != "C" &&
                answerOne != "K" && answerOne !=
                "Q") {
              print("Please enter C (choose), K (Keep) or Q (Quit).")
            }
            if (answerOne == "Q") {
              return()
            }
            if (answerOne == "C") {
              chosen = FALSE
              while (chosen == FALSE) {
                message = paste("You may choose one between",
                                theModels,
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
            if (answerOne == "K") {
              answered = TRUE
            }
          }
        }
        ### If we are not in interactive (ask=TRUE) mode
        ### or if the user has so chosen, we keep the 
        ### reconstructed model
        if(ask==FALSE || answerOne == "K"){
          # NB: we need || instead of | to stop evaluation if first
          # test is true (avoid answerOne not existing with ask FALSE)
          colnames(modelsByGroup)[i] = labelMyMss
          modelsByGroup[, i] = labelMyModel  
          # models[[i]] = myModel 
          #This seems to create a weird bug 
          # (again with R and its object classes...).
          # I'll try this for the moment :
          modelsToAdd[[i]] = as.matrix(myModel)
          keepVirtualModel = TRUE
        }
      }
      if (length(extantModel) == 1 && keepVirtualModel == FALSE) {
        if (verbose) {
          cat(
            extantModel,
            "is the only ms. inside the group that seems to be the model of group",
            labelMyMss, "\n"
          )
        }
        colnames(modelsByGroup)[i] = labelMyMss
        modelsByGroup[, i] = extantModel
        # option to recover NAs when a ms. of the database is identified with a just reconstructed model
        #first we test if the option is activated and NA's actually exist in the extant model
        if (recoverNAs == TRUE &&
            (length(which(is.na(
              tableVariantes[, extantModel]
            ))) > 0)) {
          for (r in 1:nrow(tableVariantes)) {
            # for each reading, if it is NA in the ms. but not in the reconstructed model, we replace it.
            if ((is.na(tableVariantes[r, extantModel])) &&
                !is.na(myModel[r, ])) {
              # Debug:
              if(verbose){
                cat("Recovering reading of virtual model", 
                    labelMyModel, "at VL", r, "Value was", 
                    tableVariantes[r, extantModel], 
                    "will be", myModel[r,],"\n")
                }
              tableVariantes[r, extantModel] = myModel[r, ]
            }
          }
        }
      }
      if (length(extantModel) == 0 && keepVirtualModel == FALSE) {
        # No ms. inside this group corresponds. We proceed to compare with mss
        # outside the group. 
        # NB & TODO(GLOBAL): this step (that we included in
        # the paper) is PROBABLY not necessary, nor algorithmically consistent.
        # How can the model be outside the group and
        # have no disagreement with the model, knowing that the virtual model is
        # reconstructed based on common readings to the mss of the group, and
        # that these are, at least once, unique to this group? Yet the complexity
        # of this principle is very high, and intuition hard, so we need to
        # check it.
        # TODO: this also creates a lot of redundancy in code. Clean it up?
        if (verbose) {
          writeLines(
            paste(
              "No ms inside group",
              labelMyMss,
              "seems to be the model.\nWe will proceed to a comparison with mss outside the group."
            )
          )  #NB: si nous voulons être rigoureux, il faut que la base de données inclue également les mss retirés aux étapes précédentes?
        }
        # We create a database containing the model and the mss outside the group
        others = colnames(tableVariantes)[!colnames(tableVariantes) %in%
                                            myGroup]
        # if there are still other manuscripts
        if (length(others) > 0) {
          othersAndModel = cbind(tableVariantes[, others, drop = FALSE],
                                 myModel)  # We compare them 
          myOthersComp = PCC.disagreement(othersAndModel)
          for (n in 1:length(others)) {
            if (myOthersComp$severeDisagreement[others[n], labelMyModel] ==
                0 &&
                myOthersComp$benignDisagreement[others[n], labelMyModel] ==
                0 &&
                (
                  myOthersComp$omissionsOriented[others[n], labelMyModel] ==
                  0 |
                  is.na(myOthersComp$omissionsOriented[others[n], labelMyModel])
                )) {
              if (verbose) {
                cat(others[n], "seems to be the model.\n")
              }
              extantModel = c(extantModel, others[n])
            } else {
              if (verbose) {
                cat(
                  others[n],
                  "has",
                  myOthersComp$severeDisagreement[others[n],
                                                  labelMyModel],
                  "severe disagreement(s),",
                  myOthersComp$benignDisagreement[others[n],
                                                   labelMyModel],
                  "benign disagreement(s),",
                  myOthersComp$omissionsOriented[others[n],
                                                 labelMyModel],
                  "omissions",
                  "towards the virtual model.\nIt does not seem to be the model\n"
                )
              }
            }
          }
          # Now that the comparison is done, we verify that the results are
          # consistent, and proceed to affect the role of model to the good ms.
          if (length(extantModel) > 1) {
            extantModel = paste(extantModel, collapse = "")
            stop(
              paste(
                "It appears that more than one manuscript (",
                extantModel,
                ") can be the model of group ",
                labelMyMss,
                ".\nThere is not enough data to decide.\n Stemma building will stop NOW.",
                sep = ""
              )
            )
          }
          if (length(extantModel) == 1) {
            if (verbose) {
              cat(extantModel, "seems to be the model of this group\n")
            }
            colnames(modelsByGroup)[i] = labelMyMss
            modelsByGroup[, i] = extantModel
            # option to recover NAs when a wit. of the database is 
            # identified with a just reconstructed model
            # first we test if the option is activated and NA's 
            # actually exist in the extant model
            if (recoverNAs == TRUE &&
                (length(which(is.na(
                  tableVariantes[, extantModel]
                ))) > 0)) {
              for (r in 1:nrow(tableVariantes)) {
                # for each reading, if it is NA in the wit. 
                # but not in the reconstructed model, we replace it.
                if ((is.na(tableVariantes[r, extantModel])) &&
                    !is.na(myModel[r, ])) {
                  tableVariantes[r, extantModel] = myModel[r, ]
                }
              }
            }
          }
        } else {
          if(verbose){print("There are no other manuscript left in the database.\n")}
        }
        if (length(extantModel) == 0) {
          # If length is STILL equal to 0, then the manuscript is lost, and we keep
          # the virtual model
          if (verbose) {
            writeLines(
              paste(
                "No extant ms. at all seems to be the model of the group",
                labelMyMss,
                ".\nIt is presumably a lost ms."
              )
            )
          }
          colnames(modelsByGroup)[i] = labelMyMss
          modelsByGroup[, i] = labelMyModel
          modelsToAdd[[i]] = as.matrix(myModel)
        }
      }
      ### Here we create the edgelist. 
      ### for each manuscript in the group
      for (p in 1:length(myGroup)) {
        # if he is not the model
        if (myGroup[p] != modelsByGroup[i]) {
          # we add a link between the model and him in the edgelist
          # as well as a calculation of distance:
          # total number of disagreements, and omissions both ways
          
          myDist = c(
            myGroupComp$benignDisagreement[myGroup[p],modelsByGroup[i]],
            myGroupComp$omissionsOriented[myGroup[p],modelsByGroup[i]],
            myGroupComp$omissionsOriented[modelsByGroup[i],myGroup[p]])
          myDist[is.na(myDist)] = 0
          myDist = sum(myDist)
          edgelist = rbind(edgelist, c(modelsByGroup[i], myGroup[p], myDist))  
          # we add the wit. to the descripti (to be removed) list
          descripti = c(descripti, myGroup[p])
        }
      }
      modelsReconstructed = cbind(modelsReconstructed,myModel)
    }
    # modelsToAdd is the list containing the database for each virtual model, if
    # and only if the virtual model could not be identified with an existing
    # one. In that case, we need to add this virtual model to the database for
    # further calculation.
    if (length(modelsToAdd) > 0) {
      for (x in 1:length(modelsToAdd)) {
        tableVariantes = cbind(tableVariantes, modelsToAdd[[x]])
      }
    }
    # Preparing the output
    output = as.list(NULL)
    output$fullDatabase = tableVariantes 
    # the new database
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
      #NB: when you select from an object, R, coerces it to the lowest
      # possible form by default. That is, when you select one (and only one)
      # column from a matrix, it suddenly becomes a vector without colname...
      # So, to avoid that, you have to set ,drop = FALSE
      database = tableVariantes[, nonDescripti, drop = FALSE]
    }
    output$database = database  
    # the edgelist
    # Debug: plot the stemma
    #if(verbose){
    #  myNetwork = igraph::graph_from_edgelist(edgelist, directed = TRUE)
    #  igraph::plot.igraph(myNetwork, layout=layout_as_tree)
    #}
    output$edgelist = edgelist  # and the rest
    output$models = modelsReconstructed
    output$modelsByGroup = modelsByGroup
    return(output)
  }

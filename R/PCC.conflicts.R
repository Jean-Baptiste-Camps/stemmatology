PCC.conflicts <-
  function(x,
           omissionsAsReadings = FALSE,
           alternateReadings = FALSE) {
    options(stringsAsFactors = FALSE) 
    # TODO: perhaps better, for alternateReadings, to create as many
    # new rows as necessary, at the start, and avoid code redundancy
    # as well as many small inflexions to the code due to the fact
    # that we need to cross VL with themselves if alternate.
    # it could also make the network more legible.
    # This could even be ported into a different function, 
    # for separate use.
    # Except that it is not what we need
    # Option to avoid using factor and gaining efficiency
    # Perhaps better to do:
    if (!is.matrix(x)) {
      stop("Please input a matrix")
    }
    tableVariantes = x
    # Initial test to verify if the input is what it is supposed to be
    if (!is.numeric(tableVariantes) &
        alternateReadings == FALSE) {
      stop(
        "The input database is not a numeric matrix. If it is a character matrix\ncontaining alternate readings please set alternateReadings to TRUE.\nOtherwise, try converting it to a numeric matrix object."
      )
    }
    if (!is.character(tableVariantes) &
        alternateReadings == TRUE) {
      stop(
        "The input database is not a character matrix. If it is a numeric matrix\n(i.e. not containing alternate readings) please set alternateReadings to FALSE"
      )
    }
    # TODO: verify if rownames are present?
    # Preparing the objects we will need later
    tableVariantesInitial = tableVariantes
    # Create an edgelist for conflicts
    edgelist = matrix(c(character(0), character(0)), ncol = 2)
    # Create a matrix of conflicts per VL
    conflictsTotal = matrix(
      data = 0,
      nrow = nrow(tableVariantes),
      ncol = 1,
      dimnames = list(rownames(tableVariantes), "conflictsTotal")
    )
    # Treating the case of omissions
    if (omissionsAsReadings == FALSE) {
      # Standard case: omissions are not considered as potential common errors
      tableVariantes[tableVariantes == 0] = NA
    }
    # We need to consider even the case where there is only
    # one row, because it can contradict itself with 'alternateReadings'
    for (i in seq_len(nrow(tableVariantes))) {
      # the problematic configuration can only happen if there are 
      # at least two unique values for each VL (i and j). 
      # So we test for i (if not, we can stop here), and then for j
      VLA = as.vector(tableVariantes[i, , drop = TRUE])
      # here a function to treat the case where there are alternate readings, 
      # comma separated, for a single manuscrit
      if (alternateReadings == TRUE) {
        toBeRemovedForA = NULL # the entry containing the splitted values (with ","), 
        # that will be removed after splitting, and are necessary for establishing 
        # the new values of B
        splittedInForA = NULL # the number of time each values was splitted
        newValuesForA = NULL # and the new added
        hasBeenSplittedA = FALSE # a test to check afterwards if A has been modified
        # and to vectors we need for modifying B if A has been modified
        for (l in seq_len(length(VLA))) {
          if (!is.na(VLA[l])) {
            #if (length(grep(",", VLA[l], fixed = TRUE)) > 0) {
            if (grepl(",", VLA[l], fixed = TRUE)) {
              # fixed = TRUE means we are not using a regexp. It is also much faster.
              newA = unlist(strsplit(VLA[l], split = ",", fixed = TRUE))
              splittedInForA = c(splittedInForA, length(newA))
              newValuesForA = c(newValuesForA, newA)
              toBeRemovedForA = c(toBeRemovedForA, l)
            }
            #}
          }
        }
        if (!is.null(toBeRemovedForA)) {
          hasBeenSplittedA = TRUE
          VLA = VLA[-toBeRemovedForA]
          VLA = c(VLA, newValuesForA)
        }
        VLA = as.numeric(VLA)
        if (omissionsAsReadings == FALSE) {
          # Standard case: omissions are not considered as potential common errors
          VLA[VLA == 0] = NA
        }
        # We need to save the VLA object, because it will be modified later
        savedVLA = VLA
        hasBeenModifiedAbyB = FALSE # for the moment, changes in B have not affected A
      }
      if (length(unique(VLA[!is.na(VLA), drop = TRUE])) > 1) {
        # We store the row and its unique values in a vector, 
        # because we will need them frequently
        uniqueVLA = unique(VLA[!is.na(VLA)])
        for (j in i:nrow(tableVariantes)) {
          # Same test for j here
          VLB = as.vector(tableVariantes[j, , drop = TRUE])
          if (alternateReadings == TRUE) {
            # First we check if A has been modified, and if it has been, we modify B accordingly (only B for now)
            if (hasBeenSplittedA == TRUE) {
              newValuesForB = NULL
              for (index in 1:length(toBeRemovedForA)) {
                newValuesForB = c(newValuesForB, rep(VLB[toBeRemovedForA[index]], splittedInForA[index])) # we take the values of B corresponding to the rows of A that have been splitted, and multiply them by the number of values it was splitted in, to keep the correspondence
              }
              VLB = VLB[-toBeRemovedForA]
              VLB = c(VLB, newValuesForB)
            }
            newValuesForB = NULL
            toBeRemovedForB = NULL
            splittedInForB = NULL
            hasBeenSplittedB = FALSE
            for (l in 1:length(VLB)) {
              if (!is.na(VLB[l])) {
                #if (nchar(VLB[l]) > 1) {
                if (grepl(",", VLB[l], fixed = TRUE)) {
                  newB = unlist(strsplit(VLB[l], split = ",", fixed = TRUE))
                  splittedInForB = c(splittedInForB, length(newB))
                  newValuesForB = c(newValuesForB, newB)
                  toBeRemovedForB = c(toBeRemovedForB, l)
                }
                #}
              }
            }
            if (!is.null(toBeRemovedForB)) {
              hasBeenSplittedB = TRUE
              VLB = VLB[-toBeRemovedForB]
              VLB = c(VLB, newValuesForB)
            }
            VLB = as.numeric(VLB)
            if (omissionsAsReadings == FALSE) {
              # Standard case: omissions are not considered as potential common errors
              VLB[VLB == 0] = NA
            }
          }
          if (length(unique(VLB[!is.na(VLB), drop = TRUE])) > 1) {
            # Last specificity for the treatment of alternate readings, 
            # create the two updated rows, in a corresponding fashion. 
            #To do that, we have to go back a step...
            # it will be dirty
            if (alternateReadings == TRUE) {
              # Finally, if B has been modified, we modify A accordingly
              # First, if A has been modified during a previous comparison, we need to set it back to its original value
              if (hasBeenModifiedAbyB == TRUE) {
                VLA = savedVLA
              }
              if (hasBeenSplittedB == TRUE) {
                newValuesForA = NULL
                for (index in 1:length(toBeRemovedForB)) {
                  newValuesForA = c(newValuesForA, rep(VLA[toBeRemovedForB[index]], splittedInForB[index]))
                }
                VLA = VLA[-toBeRemovedForB]
                VLA = c(VLA, newValuesForA)
                hasBeenModifiedAbyB = TRUE
              }
            }
            # Et c'est ici que les problèmes commencent
            # We look for configuration such as, given a first , 
            # variant location containing {x, x', ..} and a second {y, y', ... }, 
            # for each combination {x, y} € m_k and {x, y'} € m_k, 
            # is there a x' such as {x', y} € m_k and {x', y'} € m_k
            problematicConfiguration = FALSE # FALSE until proven otherwise
            # for each x
            # and as long as a problematic configuration has not been found
            for (x in 1:length(uniqueVLA)) {
              #debug:
              #print(paste("Now testing uniqueVLA 1", uniqueVLA[x]))
              #/debug
              if (problematicConfiguration == TRUE) {
                break()
              }
              # is there at least to y's (omitting NA's) ?
              if (length(unique(stats::na.omit(VLB[which(VLA == uniqueVLA[x])]))) > 1) {
                # if there is, lets keep it
                indexOfxy = which(VLA == uniqueVLA[x])
                uniqueVLBmatchingx = unique(VLB[indexOfxy])
                uniqueVLBmatchingx = uniqueVLBmatchingx[!is.na(uniqueVLBmatchingx)]
                # And we create two vectors containing the readings for each variant location, minus the ones for which an association was already computed (minus the {x, y}'s and {x, y'}'s)
                newVLA = VLA[-indexOfxy]
                newVLB = VLB[-indexOfxy]
                # Now for a final test : we verify that there are at least two of the y's associated with x appearing inside the group of the y's not associated with x's
                if (length(uniqueVLBmatchingx[uniqueVLBmatchingx %in% newVLB]) > 1) {
                  # Ok, we are good to go
                  # and, finally, we create a list of unique values of the second variant locations that are both associated with x and with another one (that is, the y's that have other associations than x to themselves)
                  uniqueNewVLB = uniqueVLBmatchingx[uniqueVLBmatchingx %in% newVLB]
                  # and a vector, in which each entry corresponds to a unique value excepted x (an x', x'', ...) of the first location
                  myMatches = vector(mode = "numeric", length = (length(uniqueVLA)))
                  names(myMatches) = uniqueVLA
                  # Now for the hard part. We have to see if at least two y have an x' (other than x) in common
                  for (y in 1:length(uniqueNewVLB)) {
                    myNewMatches = unique(stats::na.omit(newVLA[which(newVLB == uniqueNewVLB[y])]))
                    if (length(myNewMatches) > 0) {
                      # Very important test, to verify we are not going to bind a NA column to myMatches...
                      for (m in 1:length(myNewMatches)) {
                        myMatches[as.character(myNewMatches[m])] = myMatches[as.character(myNewMatches[m])] +
                          1
                      }
                    }
                    if (length(myMatches[myMatches > 1]) > 0) {
                      problematicConfiguration = TRUE
                      break()
                    }
                  }
                }
              }
            }
            if (problematicConfiguration == TRUE) {
              #adding a new edge to the network of conflicts !
              edgelist = rbind(edgelist, c(
                rownames(tableVariantes)[i],
                rownames(tableVariantes)[j]
              ))
              # trying not to use rbind => does not cause a gain
              #edgelist = c(edgelist, c(rownames(tableVariantes)[i], rownames(tableVariantes)[j]))
              conflictsTotal[i,] = conflictsTotal[i,] + 1
              conflictsTotal[j,] = conflictsTotal[j,] + 1  #ajouter le nombre de conflits pour chacun des deux lieux variants
            }
          }
        }
      }
    }  # end of crossing with a second variant location
    # And here we create the edgelist matrix if we try not to use rbind
    #         if (!is.null(edgelist)) {
    #             edgelist = matrix(edgelist, ncol = 2, nrow = (length(edgelist)/2) , byrow = TRUE)
    #         }
    centrality = conflictsTotal  ##Computing the centrality index as described in CC 2013
    # We have to test first that there actual are conflicts in the database
    if (sum(conflictsTotal) > 0) {
      sumConflicts = sum(conflictsTotal) / 2
      for (z in 1:nrow(centrality)) {
        # Another test, to avoid division by zero (perhaps the computation of the
        # centrality index should be adapted. Discuss this with Florian. Or, we
        # could accept to have infinite numbers... does it makes sense ? They
        # sure are superior to any centrality threshold we could choose... if()
        centrality[z,] = centrality[z,] / (sumConflicts - centrality[z,])  # added an option to remove infinity and to replace it with 2
        if (is.infinite(centrality[z,])) {
          centrality[z,] = 2
        }
      }
    } else {
      for (z in 1:nrow(centrality)) {
        centrality[z,] = centrality[z,] = 0
      }
    }
    ## Now we create the objects that the function will return (a list
    ## containing edgelist, conflictsTotal and original database
    output = as.list(NULL)
    output$edgelist = edgelist
    output$conflictsTotal = cbind(conflictsTotal, centrality)
    colnames(output$conflictsTotal) = c("Number of conflicts", "Centrality index")  #output$database = tableVariantes #La base des variantes, donnée en entrée, et dont a besoin pour PCC.contam #Pourrait être remplacé par une option dans la fonction PCC.contam #Il faudrait mieux reprendre x, pour éviter de virer les omissions
    ## Il faudrait insérer ici des commandes pour retirer tous les objets dont
    ## on n'a plus besoin.
    output$database = tableVariantesInitial  #And now we proceed to draw the plot
    # We have to add a test here, since in some extreme cases, the edgelist
    # can in fact be empty
    if (length(edgelist) != 0) {
      # switching to igraph
      #myNetwork = as.network(edgelist, directed = FALSE, matrix.type = "edgelist")  #Important remark here : not specifying matrix.type = edgelist gave, occasionnaly, weird errors, mainly 'Erreur dans abs(x) : argument non numérique pour une fonction mathématique'... So, I am expliciting this option everywhere
      #gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork),
      # gmode = "graph", boxed.labels = TRUE)
      myNetwork = igraph::graph_from_edgelist(edgelist, directed = FALSE)
      myLayout = igraph::layout_with_fr(myNetwork)
      # Color of vertices
      igraph::V(myNetwork)$color = 'white'
      # TODO: adjust label cex?
      igraph::plot.igraph(myNetwork,
                          layout = myLayout,
                          vertex.label.cex = 0.7,
                          main = 'Conflicting variant locations'
                          )
    } else {
      message("There is absolutely no conflicts in this database.")
    }
    class(output) = "pccConflicts"
    return(output)
  }

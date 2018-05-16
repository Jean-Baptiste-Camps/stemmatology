PCC.overconflicting <-
  function(x, ask = TRUE, threshold = NULL) {
    # Identifies overconflicting variant locations,
    # by helping to assess a threshold in centrality
    # and then labelling the output.
    # Input: an object of class PCC.conflicts
    # We start by checking if input is consistent
    if (class(x) != "pccConflicts") {
      stop("Input must be a pccConflicts object.")
    }
    # And options
    if (ask == FALSE) {
      if (is.null(threshold)) {
        stop("You must specify a threshold if not in interactive mode (ask = FALSE).")
      } else{
        if (!is.numeric(threshold)) {
          stop("Threshold must be a numeric value.")
        }
      }
      # TODO: further checks of consistency of threshold value?
    }
    data = x
    ordConflTot = data$conflictsTotal[order(data$conflictsTotal[, 1],
                                            decreasing = TRUE),]
    # We start with a few visualisation (barplots) to help the user choose
    # a centrality threshold, with the additional help of a clustering,
    # by partitioning around medoids with
    # 4 classes, by colouring bar according to this clustering.
    # NB: for now, we use default values, and could try variations
    # (euclid ou manhattan, ...).
    # But, according to the tests, results are robust to these variations.
    # Just test before that there is more than three
    # individuals in the database. If not, warn the user and use three
    # classes only
    testClasses = ordConflTot[, 1]
    if (length(testClasses[testClasses > 0]) > 3) {
      numberOfClasses = 4
    } else {
      if (length(testClasses[testClasses > 0]) > 1) {
        message("The number of conflicts is VERY LOW. Is your database correct?")
        numberOfClasses = length(testClasses[testClasses > 0])
      }
      if (length(testClasses[testClasses > 0]) == 0) {
        stop("There is no conflict in this database.")
      }
    }
    rm(testClasses) # don't need it anymore
    classes1 = cluster::pam(ordConflTot[, 1], numberOfClasses)
    graphics::barplot(
      ordConflTot[, 1],
      col = classes1$clustering,
      main = "Total conflicts by variant location",
      names.arg = rownames(ordConflTot),
      xlab = "VL",
      ylab = "Total conflicts",
      ylim = (c(0, ordConflTot[1, 1])),
      cex.axis = "1",
      sub = paste("coloured according to pam with", numberOfClasses, "clusters"),
      yaxt = "n"
    )
    graphics::axis(side = 2, at = seq(0, ordConflTot[1, 1], by = 2))
    #Arrêt, et demander à procéder jusqu'au second graphique
    if (ask == TRUE) {
      cat("Press [enter] to continue")
      line = readline()
    }
    classes2 = cluster::pam(ordConflTot[, 2], numberOfClasses)
    graphics::barplot(
      ordConflTot[, 2],
      col = classes2$clustering,
      main = "Centrality by variant location",
      names.arg = rownames(ordConflTot),
      xlab = "VL",
      ylab = "Centrality Index",
      ylim = (c(0, ordConflTot[1, 2])),
      cex.axis = "1",
      sub = paste("coloured according to pam with",
                  numberOfClasses, "clusters"),
      yaxt = "n"
    )
    graphics::axis(side = 2, at = seq(0, ordConflTot[1, 2], by = 0.02))
    if (ask == TRUE) {
      cat("Press [enter] to continue")
      line = readline()
    }
    ## We show centrality distribution visually. Other methods possible?
    ## Perhaps, by clustering, could we suggest a value.
    ## See also k-means, scale, etc. ?
    ## For the moment, pam works very well with 4 classes, at least
    ## on Chevalier au Lyon
    ## (makes sense : 1 class for over-conflicting, one for non
    ## assessables, one for sobers, and one for non-conflicting).
    ### With 3 classes, works very well too, in the sense that the 2
    ### firs clusters group together.
    ### Maybe look at ClustOfVar (http://arxiv.org/pdf/1112.0295v1.pdf)
    myNetwork = igraph::graph_from_edgelist(data$edgelist,
                                            directed = FALSE)
    
    # Let's set a centrality threshold, if it wasn't defined yet
    if (ask == TRUE) {
      answered = FALSE
      while (answered == FALSE) {
        threshold = as.numeric(readline("Choose a centrality threshold > "))
        if (is.na(threshold)) {
          print("Please enter a number.")
        } else {
          if (threshold >= 2) {
            #TODO: modify if the formula changes
            print("Please enter a number inferior to 2 (which is the maximum possible value).")
          } else {
            answered = TRUE
          }
        }
      }
    }
    # To avoid using unnecessary factors
    options(stringsAsFactors = FALSE)
    # Create a table of labels for nodes (vertexAttributes)
    hasConflicts = data$conflictsTotal[data$conflictsTotal[, 2] > 0, , drop = FALSE]
    vertexAttributes = matrix(
      nrow = nrow(hasConflicts),
      ncol = 2,
      dimnames = list(rownames(hasConflicts), c("label", "color"))
    )
    # And label nodes according to centrality threshold
    vertexAttributes[hasConflicts[, 2] > threshold, 1] = "overconflicting"
    vertexAttributes[hasConflicts[, 2] > threshold, 2] = "red"
    vertexAttributes[hasConflicts[, 2] <= threshold &
                       hasConflicts[, 2] > 0, 1] = "unknown"
    vertexAttributes[hasConflicts[, 2] <= threshold &
                       hasConflicts[, 2] > 0, 2] = "grey"
    # And now for labelling 'sobers'
    # Loop on nodes to be assessed
    toAssess = vertexAttributes[vertexAttributes[, 1] != "overconflicting", , drop = FALSE]
    for (i in seq_len(nrow(toAssess))) {
      # If the node is linked to a node that isn't overconflicting
      # Get the neighbors
      myNeighbors = igraph::neighbors(myNetwork, rownames(toAssess)[i])
      # if there is no link to something else than an overconflicting
      if (!"unknown" %in% vertexAttributes[myNeighbors$name, 1]) {
        vertexAttributes[rownames(toAssess)[i],] = c("sober", "green")
      }
    }
    rm(toAssess)
    # And now, we give colour attributes to nodes
    igraph::V(myNetwork)[rownames(vertexAttributes)]$color = vertexAttributes[, 2]
    # And plot
    myLayout = igraph::layout_with_fr(myNetwork)
    igraph::plot.igraph(
      myNetwork,
      layout = myLayout,
      vertex.label.cex = 0.7,
      main = 'Conflicting variant locations'
    )
    # we add the vertexAttributes to the pccConflicts object inputed, and
    # return it as a pccElimination object
    data$vertexAttributes = vertexAttributes
    class(data) = "pccOverconflicting"
    return(data)
  }

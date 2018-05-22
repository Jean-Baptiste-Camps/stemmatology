PCC.equipollent <-
  function(x,
           ask = TRUE,
           scope = NULL,
           wits = NULL,
           verbose = FALSE) {
    # In input, a PCC.conflicts object, preferably after a first elimination
    # of non-genealogical variant locations. For the nodes that are not
    # algorithmically decidable, it allows to create alternative databases
    # that can represent equipollent configurations ou cases of contamination.
    # The database can be globally deduplicated in as much internally
    # consistent configurations as there are, or by duplicating only
    # problematic witnesses (identified with PCC.contam).
    #
    # Verify input
    if (class(x) != "pccConflicts") {
      stop('Input must be a pccConflicts object.')
    }
    # Verify consistency of options
    if (ask == FALSE) {
      if (is.null(scope)) {
        stop('You must specify scope in non interactive mode (ask = FALSE).')
      } else{
        if (scope != 'T' & scope != 'W') {
          stop('scope must be equal to "T" or "W"')
        } else {
          if (scope == 'W' & is.null(wits)) {
            stop('you must specify wits with scope="W"')
          } else{
            if (!all(wits %in% colnames(x$database))) {
              stop('all witnesses in the value of wit must be in the database.')
            }
          }
        }
      }
    }
    # Verify that there are conflicts to start with
    if(nrow(x$edgelist) == 0){
      stop("No conflicts in input. PCC.equipollent will stop.")
    }
    # First, we need an adjacency matrix
    myNetwork = igraph::graph_from_edgelist(x$edgelist, directed = FALSE)
    database = x$database
    #For each node, we want the nodes with which there are no direct links
    adjacencyTable = igraph::as_adjacency_matrix(myNetwork, sparse = FALSE)
    notInConflict = as.list(NULL)
    for (i in 1:nrow(adjacencyTable)) {
      notInConflict[[i]] = colnames(adjacencyTable[i, adjacencyTable[i, ] == 0, drop = FALSE])
      # Modified code to compare only to half the table, since it is symetrical
      # When we will put this version in function, we can also remove the next for loop,
      # since there will be no identical lists
      # cols =  which(adjacencyTable[i, ] == 0,  arr.ind = TRUE)
      # cols = cols[cols >= i]
      # notInConflict[[i]] = colnames(adjacencyTable[i, cols, drop = FALSE])
      # /!\: I think that, if we do that, we will loose some (potentiall weird)
      # configurations
    }
    # Remove identical lists
    notInConflict = unique(notInConflict)
    # And here, we add a final test to check if there are unvalid
    # configurations in which some members are in conflict between
    # themselves, in which case we will remove them.
    # In order for that to work, we need to deduplicate problematic
    # configurations into unproblematic ones… Harder than it looks
    toBeRemovedAsWell = as.vector(NULL)
    #If there is more than two members
    for (l in seq_len(length(notInConflict))) {
      # If there are more than two members
      if (length(notInConflict[[l]]) > 2) {
        # We test for problematic configurations
        # TODO(JBC): problematic for some traditions
        # and to be adapted, see
        # https://github.com/Jean-Baptiste-Camps/stemmatology/issues/7
        problems = FALSE
        for (m in 1:(length(notInConflict[[l]]) - 1)) {
          for (n in (m + 1):length(notInConflict[[l]])) {
            # We confront them in the adjacencyTable
            if (adjacencyTable[notInConflict[[l]][m], notInConflict[[l]][n]] >
                0) {
              problems = TRUE
              break()
            } else {
              problems = FALSE
            }
          }
          if (problems == TRUE) {
            break()
            #break also from the upper loop
          }
        }
        if (problems == TRUE) {
          if (verbose) {
            cat(
              "There is a weird configuration; we will remove this group from the list alltogether.\n",
              "It concerns VL:\n",
              notInConflict[[l]],
              "\n"
            )
          }
          toBeRemovedAsWell = c(toBeRemovedAsWell, l)
        }
      }
    }
    # And finally, we remove them.
    if (!is.null(toBeRemovedAsWell)) {
      notInConflict = notInConflict[-toBeRemovedAsWell, drop = FALSE]
    }
    # Now, decide if we want to deduplicate for all witnesses, or only
    # some of them.
    if (ask) {
      cat("The group(s) of VL without internal conflicts are :")
      print(notInConflict)
      cat(
        "Do you wish to separate them for the whole tradition [T]\n or only for some (presumably contaminated) witnesses [W]\n - or not at all (press any other key)?"
      )
      scope = readline("(T/W/q)")
      if (scope == "W") {
        wits = unlist(strsplit(
          readline(
            "Please enter the witness(es) siglum(-a) (space separated list) \n"
          ),
          "\\s+",
          perl = "TRUE"
        ))
      }
    }
    databases = as.list(NULL)
    if (scope == "T") {
      # We create as many alternatives as there are unconflicting
      # configurations And we delete every conflicting row, **except for the
      # ones contained in our configuration**
      for (o in seq_len(length(notInConflict))) {
        delete = rownames(adjacencyTable)[!rownames(adjacencyTable) %in%
                                            notInConflict[[o]]]
        databases[[o]] = database[!rownames(database) %in% delete,
                                  , drop = FALSE]
      }
    }
    if (scope == "W") {
      # The same, but only for one or several manuscript
      for (o in seq_len(length(notInConflict))) {
        delete = rownames(adjacencyTable)[!rownames(adjacencyTable) %in%
                                            notInConflict[[o]]]
        databases[[o]] = database
        databases[[o]][rownames(database) %in% delete, wits] = NA
      }
    }
    
    # Preparing output
    # An object of class pccEquipollent, a list containing
    #  1. a list with all alternative databases that have been created, if any
    #  2. a list with The group(s) of VL without internal conflicts
    output = as.list(NULL)
    output$databases = databases
    output$notInConflict = notInConflict
    class(output) = "pccEquipollent"
    return(output)
  }

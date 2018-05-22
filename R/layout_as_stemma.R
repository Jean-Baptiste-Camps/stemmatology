layout_as_stemma <-
  function(x) {
    # A function to create an igraph layout for stemma, where
    # witnesses as placed at a vertical distance from their parent
    # consistent with the number of disagreements and omissions
    # TODO: try to avoid horizontal overlapping of vertices
    # # # VOIR SI ON VEUT ÇA x : a directed igraph graph
    # x : an edgelist with distances in third column
    #myNetwork = x
    edgelist = x
    
    myNetwork = igraph::graph_from_edgelist(edgelist[, 1:2], directed = TRUE)
    
    # Let's find roots
    roots = which(sapply(sapply(igraph::V(myNetwork),function(x) igraph::neighbors(myNetwork,x, mode="in")), length) == 0)
    
    myLayout = igraph::layout_as_tree(myNetwork, root = roots, mode = "out")
    
    #First, it's good to have a topological sort, to have root first
    mySortedNet = igraph::topo_sort(myNetwork, mode = "out")
    
    for (i in seq_len(length(mySortedNet))) {
      # For each node, we get connection and weight from
      # the input edgelist with it's label
      for (j in seq_len(length(edgelist[, 1][edgelist[, 1] == mySortedNet[i]$name]))) {
        # We look towards wich node it is connected, and adjust their
        # (vertical) position using the edgelist
        # the new position is equal to the parent node position - dist from him
        # (we need substraction to go top -> down)
        # N.B.: with this calculation, a wit. derived from several models
        # will be placed according to its distance with the last parent
        # in the topological sort
        # To get parent pos, we need to get its numeric index from its name
        # Warning: this is going to get a bit ugly with cross-references
        newPos =
          myLayout[as.numeric(V(myNetwork)[mySortedNet[i]$name]) , 2] -
          as.numeric(edgelist[, 3][edgelist[, 1] == mySortedNet[i]$name][j])
        # NB: as.numeric is used to get node index from its name
        # and modify position of the children
        myLayout[as.numeric(V(myNetwork)[edgelist[, 2][edgelist[, 1] == mySortedNet[i]$name][j]])
                 , 2] = newPos
      }
    }
    # # And now, let's try to fix superpositions
    # # first, let's take all unique horizontal coords
    # myYs = sort(unique(myLayout[, 2]))
    # for (i in seq_len(length(myYs))) {
    #   # Do we have more than one node?
    #   if (length(myLayout[, 2][myLayout[, 2] == myYs[i]]) > 1) {
    #     # Are some of them at the same horizontal placement?
    #     myXs = sort(unique(myLayout[, 1][myLayout[, 2] == myYs[i]]))
    #     # if we have less unique value than values
    #     if (length(myXs) < length(myLayout[, 1][myLayout[, 2] == myYs[i]])) {
    #       # for each values, let's see if it is close to another node
    #       # (within myRange)
    #       myRange = 0.2
    #       for (j in seq_len(length(myXs))) {
    #         if (length(myLayout[, 1][myLayout[, 2] == myYs[i] &
    #                                  (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                   myLayout[, 1] <= (myXs[j] + myRange))])
    #             > 1) {
    #           # And now, it becomes hard: let's try to avoid overlap
    #           # without creating new overlaps
    #           for (k in seq_len(length(myLayout[, 1][myLayout[, 2] == myYs[i] &
    #                           (myLayout[, 1] >= (myXs[j] - myRange) &
    #                           myLayout[, 1] <= (myXs[j] + myRange))]))) {
    #             # let's determine how much we want to move it, as a function
    #             # of its heigth (the higher, the more it is moved)
    #             mov = sqrt((min(myYs)- myYs[i])^2) + 1 
    #             # if it is in the first half, move it to the left, otherwise,
    #             # move it to the right
    #             myLength = length(myLayout[, 1][myLayout[, 2] == myYs[i] & 
    #                                               (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                                  myLayout[, 1] <= (myXs[j] + myRange))])
    #             if (k <= (myLength/2)){
    #               myLayout[, 1][myLayout[, 2] == myYs[i] & 
    #                               (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                  myLayout[, 1] <= (myXs[j] + myRange))][k] =
    #                 myLayout[, 1][myLayout[, 2] == myYs[i] & 
    #                                 (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                    myLayout[, 1] <= (myXs[j] + myRange))][k] - mov * (1/k)
    #             }
    #             else{
    #               myLayout[, 1][myLayout[, 2] == myYs[i] & 
    #                               (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                  myLayout[, 1] <= (myXs[j] + myRange))][k] =
    #                 myLayout[, 1][myLayout[, 2] == myYs[i] & 
    #                                 (myLayout[, 1] >= (myXs[j] - myRange) &
    #                                    myLayout[, 1] <= (myXs[j] + myRange))][k] + mov * (1/(myLength - k + 1))
    #             }
    #           }
    #         }
    #       }
    #     }
    #   }
    # }
    # A layout object to be passed to igraph
    return(myLayout)
    # Maybe should return
    # a list, with
    # graph: an igraph graph
    # layout: a layout to be passed to igraph
  }

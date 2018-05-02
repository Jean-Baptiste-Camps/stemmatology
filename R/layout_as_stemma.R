layout_as_stemma <-
  function(x) {
    # A function to create an igraph layout for stemma, where
    # witnesses as placed at a vertical distance from their parent
    # consistent with the number of disagreements and omissions
    # # # VOIR SI ON VEUT ÇA x : a directed igraph graph
    # x : an edgelist with distances in third column
    #myNetwork = x
    edgelist = x
    
    igraph::graph_from_edgelist(edgelist[,1:2], directed = TRUE)
    
    myLayout = igraph::layout_as_tree(myNetwork)
    
    #First, it's good to have a topological sort, to have root first
    mySortedNet = igraph::topo_sort(myNetwork, mode="out")
    
    for (i in seq_len(length(mySortedNet))){
      # For each node, we get connection and weight from
      # the input edgelist with it's label
      for (j in seq_len(length(edgelist[,1][edgelist[,1] == mySortedNet[i]$name]))){
        # We look towards wich node it is connected, and adjust their
        # (vertical) position using the edgelist
        # the new position is equal to the parent node position - dist from him
        # (we need substraction to go top -> down)
        # To get parent pos, we need to get its numeric index from its name
        # Warning: this is going to get a bit ugly with cross-references
        newPos = 
          myLayout[as.numeric(V(myNetwork)[mySortedNet[i]$name]) , 2] - 
          as.numeric(edgelist[,3][edgelist[,1] == mySortedNet[i]$name][j])
        # NB: as.numeric is used to get node index from its name
        # and modify position of the children
        myLayout[
          as.numeric(V(myNetwork)[edgelist[,2][edgelist[,1] == mySortedNet[i]$name][j]]) 
          , 2] = newPos
      }
    }
    # A layout object to be passed to igraph
    return(myLayout)
  }
    

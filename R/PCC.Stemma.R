PCC.Stemma <-
  function(x,
           omissionsAsReadings = FALSE,
           limit = 0,
           recoverNAs = TRUE,
           layout_as_stemma = FALSE,
           ask = TRUE,
           verbose = FALSE) {
    # TODO(JBC): la sortie de cette fonction n'a pas de classe, il faudrait 
    # l'implémenter
    # TODO(JBC): Modifier cette fonction pour pouvoir prendre en entrée une
    # liste de bases de données (ajouter une option) issue de
    # PCC.equipollent, et dans ce cas, traiter chacune des tables à son tour
    # (et fusionner les résultats), ainsi que la possibilité de prendre des
    # objets issus de PCC.elimination, etc.  Pour pouvoir faire ça, il faudra
    # être plus rigoureux dans la définition des objets.  if(is.list(x)){
    # output = as.list(NULL) # for(i in 1:length(x)){ pccStemma =
    # PCC.Stemma(x[[i]], omissionsAsReadings = omissionsAsReadings, limit =
    # limit) output[[i]] = pccStemma # } return(output) # } Deuxième
    # possibilité, le contenu n'est qu'une seule table de variantes
    # TODO: deport output creation to a function to avoid redundancy?
    # Explore graphical parameters for stemmata
    # ,
    # pad=1,
    # label.pad = 0,
    #label.pos=5,
    # label.cex=0.7
    tableVariantes = x
    edgelistGlobal = NULL # matrix(c(character(0), character(0)), ncol = 2)
    modelsGlobal = as.list(NULL)
    modelsByGroupGlobal = as.list(NULL)
    counter = 0
    while (ncol(tableVariantes) > 3) {
      counter = counter + 1
      pccdisagreement = PCC.disagreement(tableVariantes, omissionsAsReadings = omissionsAsReadings)
      pccBuildGroup = PCC.buildGroup(pccdisagreement, limit = limit, ask = ask)  # We test if no group was found
      if (identical(pccBuildGroup$groups, list())) {
        message("No group was found. Unable to build stemma.")
        # Plot the stemma at this step, if it exists
        if (!is.null(edgelistGlobal)) {
          myNetwork = igraph::graph_from_edgelist(edgelistGlobal[,1:2, drop = FALSE], directed = TRUE)
          if(layout_as_stemma){
            myLayout = layout_as_stemma(edgelistGlobal)
          }
          else{
            myLayout = layout_as_tree(myNetwork)
          }
          igraph::plot.igraph(myNetwork, layout=myLayout)
          output = as.list(NULL)
          output$edgelist = edgelistGlobal
          output$database = tableVariantes
          output$modelsGlobal = modelsGlobal
          output$modelsByGroupGlobal = modelsByGroupGlobal
          return(output)
        } else {
          return()
        }
      }
      pccReconstructModel = PCC.reconstructModel(
        pccBuildGroup,
        omissionsAsReadings = omissionsAsReadings,
        recoverNAs = recoverNAs,
        ask = ask,
        verbose = verbose
      )
      tableVariantes = pccReconstructModel$database
      if (!exists("tableVariantes")) {
        stop("No database found.")
        return(tableVariantes)
      }
      # Now we save the objects given out by PCC.reconstructModel
      edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
      modelsGlobal[[counter]] = pccReconstructModel$models
      modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
    }
    if (is.null(tableVariantes)) {
      # Job's done
      myNetwork = igraph::graph_from_edgelist(edgelistGlobal[,1:2, drop = FALSE], directed = TRUE)
      if(layout_as_stemma){
        myLayout = layout_as_stemma(edgelistGlobal)
      }
      else{
        myLayout = layout_as_tree(myNetwork)
      }
      igraph::plot.igraph(myNetwork, layout=myLayout)
      output = as.list(NULL)
      output$edgelist = edgelistGlobal
      output$database = tableVariantes
      output$modelsGlobal = modelsGlobal
      output$modelsByGroupGlobal = modelsByGroupGlobal
      return(output)
    }
    # There is now less than 4 manuscripts in the database. The method is no
    # longer really efficient, so we check if the stemma building is over,
    # and if not, we ask the user if he wants the end of the stemma
    if (ncol(tableVariantes) > 1) {
      if(ask){
        myNetwork = igraph::graph_from_edgelist(edgelistGlobal[,1:2, drop = FALSE], directed = TRUE)
        if(layout_as_stemma){
          myLayout = layout_as_stemma(edgelistGlobal)
        } else{
          myLayout = layout_as_tree(myNetwork)
        }
        igraph::plot.igraph(myNetwork, layout=myLayout)
        writeLines(
          "There is now less than four manuscripts in the database.\nStemma building has now lost in accuracy. \nDo you want to continue anyway (take last step with caution) ?\n Y/N\n"
        )
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
            answered = TRUE
          }
        }
      }
      if(ask==FALSE || answer == "Y"){
        # If not in interactive mode or if the user wishes it,
        # we complete stemma building with the last step
        counter = counter + 1
        pccdisagreement = PCC.disagreement(tableVariantes, omissionsAsReadings = omissionsAsReadings)
        pccBuildGroup = PCC.buildGroup(pccdisagreement, limit = limit, ask = ask)
        pccReconstructModel = PCC.reconstructModel(
          pccBuildGroup,
          omissionsAsReadings = omissionsAsReadings,
          recoverNAs = recoverNAs,
          ask = ask,
          verbose = verbose
        )
        tableVariantes = pccReconstructModel$database
        modelsGlobal[[counter]] = pccReconstructModel$models
        modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
        # And here, because we want dashes for the (uncertain) relations
        # established as the last step, we will create to 
        # separate networks with differente properties, before concatening
        myNetworkCert = igraph::graph_from_edgelist(edgelistGlobal[,1:2, drop = FALSE], directed = TRUE)
        # With full lines for all edges
        igraph::E(myNetworkCert)$lty = 1
        # Then dashed for the uncertain ones
        myNetworkUncert = igraph::graph_from_edgelist(pccReconstructModel$edgelist[,1:2, drop = FALSE], directed = TRUE)
        igraph::E(myNetworkUncert)$lty = 3
        # Then unite them
        myNetwork = igraph::union(myNetworkCert, myNetworkUncert, byname=TRUE)
        # fusion lty_1 et lty_2
        igraph::E(myNetwork)$lty =  ifelse(is.na(igraph::E(myNetwork)$lty_1),
                                           igraph::E(myNetwork)$lty_2,igraph::E(myNetwork)$lty_1)
        # Preparing edgelist for the output
        edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
        # We can rely on how igraph sorts vertices (i.e., using unique on the
        # edgelist turned, names <- unique(as.character(t(el))) )
        # to have the same indices for the vertices in the union
        # and in the layout function
        if(layout_as_stemma){
          myLayout = layout_as_stemma(edgelistGlobal)
        }
        else{
          myLayout = layout_as_tree(myNetwork)
        }
        # And plotting
        igraph::plot.igraph(myNetwork, layout=myLayout, main="Final stemma")
        # and output
        output = as.list(NULL)
        output$edgelist = edgelistGlobal
        output$database = tableVariantes
        output$modelsGlobal = modelsGlobal
        output$modelsByGroupGlobal = modelsByGroupGlobal
        return(output)
      }
    } else {
      output = as.list(NULL)
      output$edgelist = edgelistGlobal
      output$database = tableVariantes
      output$modelsGlobal = modelsGlobal
      output$modelsByGroupGlobal = modelsByGroupGlobal
      return(output)
    }
  }

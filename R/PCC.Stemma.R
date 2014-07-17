PCC.Stemma <-
function(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs = FALSE) {
    # TODO(JBC): dans ce groupe de fonction, comme dans le précédent, il est
    # impératif de tester que les options font bien leur job (y compris en
    # étant passées aux sous-fonctions) Première possibilité, le contenu est
    # une liste contenant plusieurs configurations alternatives. La fonction
    # s'appelle elle-même (recursivity is nice) sur chacune des composantes
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
    tableVariantes = x
    edgelistGlobal = matrix(c(character(0), character(0)), ncol = 2)
    modelsGlobal = as.list(NULL)
    modelsByGroupGlobal = as.list(NULL)
    counter = 0
    while (ncol(tableVariantes) > 3) {
        counter = counter + 1
        pccDesagreement = PCC.desagreement(tableVariantes)
        pccBuildGroup = PCC.buildGroup(pccDesagreement)  # We test if no group was found
        if (identical(pccBuildGroup$groups, list())) {
            stop("No group was found. Unable to build stemma.")
        }
        pccReconstructModel = PCC.reconstructModel(pccBuildGroup, recoverNAs = recoverNAs, omissionsAsReadings = omissionsAsReadings)
        tableVariantes = pccReconstructModel$database
        if (!exists("tableVariantes")) {
            print("Voilà qui est drôlement bizarre.")
            return(tableVariantes)
        }
        # Now we save the objects given out by PCC.reconstructModel
        edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
        modelsGlobal[[counter]] = pccReconstructModel$models
        modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
    }
    stemma = as.network(edgelistGlobal, directed = TRUE, matrix.type = "edgelist")
    gplot(stemma, displaylabels, label = network.vertex.names(stemma), gmode = "digraph", 
        boxed.labels = TRUE, usearrows = TRUE)
    if (is.null(tableVariantes)) {
        # Job's done
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
                pccReconstructModel = PCC.reconstructModel(pccBuildGroup, recoverNAs = recoverNAs, omissionsAsReadings = omissionsAsReadings)
                tableVariantes = pccReconstructModel$database
                edgelistGlobal = rbind(edgelistGlobal, pccReconstructModel$edgelist)
                modelsGlobal[[counter]] = pccReconstructModel$models
                modelsByGroupGlobal[[counter]] = pccReconstructModel$modelsByGroup
                stemma = as.network(edgelistGlobal, directed = TRUE, matrix.type = "edgelist")
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
    } else {
        output = as.list(NULL)
        output$edgelist = edgelistGlobal
        output$database = tableVariantes
        output$modelsGlobal = modelsGlobal
        output$modelsByGroupGlobal = modelsByGroupGlobal
        return(output)
    }
}

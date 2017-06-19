PCC.equipollent <-
    function(x) {
        # En entrée un objet de type PCC.conflicts Fonction qui prend en entrée,
        # à partir d'un objet de type PCC.conflicts, préférablement issu d'une
        # première élimination des variantes a-généalogiques, permet de
        # sélectionner les quelques nœuds restant en conflit entre lesquels il
        # n'est pas possible de trancher de manière algorithmique, et qui peuvent
        # représenter des configurations équipollentes ou alternatives de la
        # tradition, ou des contaminations, et permet - soit de dupliquer la base
        # en autant de configurations d'ensembles que d'ensembles de nœud sans
        # conflits entre eux (choix général) - soit de donner une seule base, en
        # en dupliquant que les mss retenus (mss suspects de contamination, et
        # qui sont responsables d'une bonne partie ou de la totalité des
        # conflits, qui ont pu être identifiés ainsi grâce à la fonction
        # PCC.contam) edgelist = x$edgelist #On commence par créer une matrice
        # d'adjacence, dont on aura besoin ensuite
        myNetwork = as.network(x$edgelist, directed = FALSE, matrix.type = "edgelist")
        adjacencyTable = as.matrix.network(myNetwork)
        database = x$database  #Pour chaque nœud, prendre tous les nœuds avec lesquels il n'est pas en conflit, et en faire la liste
        notInConflict = as.list(NULL)
        for (i in 1:nrow(adjacencyTable)) {
             notInConflict[[i]] = labels(adjacencyTable[i, adjacencyTable[i, ] == 0]) 
            # Modified code to compare only to half the table, since it is symetrical
            # When we will put this version in function, we can also remove the next for loop, since there will be no identical lists
            # cols =  which(adjacencyTable[i, ] == 0,  arr.ind = TRUE)
            # cols = cols[cols >= i]
            # notInConflict[[i]] = colnames(adjacencyTable[i, cols, drop = FALSE])
        }
        # Comparer toutes les listes et supprimer celles qui sont identiques
            toBeRemoved = as.vector(NULL)
            for (j in 1:(length(notInConflict) - 1)) {
                for (k in (j + 1):length(notInConflict)) {
                    if (identical(notInConflict[[j]], notInConflict[[k]])) {
                        toBeRemoved = c(toBeRemoved, k)
                    }
                }
            }
            if (!is.null(toBeRemoved)) {
                notInConflict = notInConflict[-toBeRemoved, drop = FALSE]
            }
        # And here, we add a final test to check if there are unvalid
        # configurations in which some members are in conflict between
        # themselves, in which case we will remove them.
        toBeRemovedAsWell = as.vector(NULL)  #If there is more than two members
        for (l in 1:length(notInConflict)) {
            # If there are more than two members
            if (length(notInConflict[[l]]) > 2) {
                # We test for problematic configurations TODO(JBC): ce code pose problème
                # sur des configurations comme celle du Chevalier au Lyon.  À VOIR AVEC
                # FLORIAN, LA COMPLEXITÉ PEUT ÊTRE PLUS GRANDE QUE PRÉVUE
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
                        break() #break also from the upper loop
                    }
                }
                if (problems == TRUE) {
                    message = paste("there is a weird configuration ; we will remove this group from the list alltogether.\n It concerns VL:")
                    writeLines(message)
                    print(notInConflict[[l]])
                    toBeRemovedAsWell = c(toBeRemovedAsWell, l)
                }
            }
        }
        # And finally, we remove them.
        if (!is.null(toBeRemovedAsWell)) {
            notInConflict = notInConflict[-toBeRemovedAsWell, drop = FALSE]
        }
        # Ensuite, demander selon si on veut dupliquer toute la base, ou
        # seulement un ms. donné, agir en fonction
        print("The group(s) of VL without internal conflicts are :")
        print(notInConflict)
        writeLines("Do you wish to separate them for the whole tradition [T]\n or only for some (presumably contaminated) manuscripts [M]\n - or not at all (press any other key)?")
        answer = readline("(T/M/q)")  ##Enter here the duplication code
        databases = as.list(NULL)
        if ((answer == "T") | (answer == "M")) {
            if (answer == "T") {
                # We create as many alternatives as there are unconflicting
                # configurations And we delete every conflicting row, **except for the
                # ones contained in our configuration**
                for (o in 1:length(notInConflict)) {
                    delete = rownames(adjacencyTable)[!rownames(adjacencyTable) %in% 
                                                          notInConflict[[o]]]
                    databases[[o]] = database[!rownames(database) %in% delete, 
                                              , drop = FALSE]
                }
            }
            if (answer == "M") {
                # The same, but only for one or several manuscript (only one implemented
                # now) Ask for the labels of the manuscripts
                mss = unlist(strsplit(readline("Please enter the manuscript siglum, or manuscripts sigla (space separated list) \n"), 
                                      "\\s+", perl = "TRUE"))
                for (o in 1:length(notInConflict)) {
                    delete = rownames(adjacencyTable)[!rownames(adjacencyTable) %in% 
                                                          notInConflict[[o]]]
                    databases[[o]] = database
                    databases[[o]][rownames(database) %in% delete, mss] = NA
                }
            }
        }
        #Préparation de la sortie: 
        #An object of class pccEquipollent, a list containing
        # 1. a list with all alternative databases that have been created, if any
        # 2. a list with The group(s) of VL without internal conflicts
        output = as.list(NULL)
        output$databases = databases
        output$notInConflict = notInConflict
        class(output) = "pccEquipollent"
        return(output)
    }

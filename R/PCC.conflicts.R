PCC.conflicts <-
function(x, omissionsAsReadings = FALSE) {
    ## V2 : Nouvelle version pour intégrer la possibilité d'avoir plusieurs
    ## variantes pour le même lieu variant/ms + index de conflictualité
    tableVariantes = as.matrix(x)  #Apparemment le tableau importé est soi disant une liste et non pas un dataframe. Donc, on tente de le contraindre
    if (omissionsAsReadings == FALSE) {
        # Standard case: omissions are not considered as potential common errors
        tableVariantes[tableVariantes == 0] = NA
    }
    # Créer une matrice (edgelist) à deux colonnes, à laquelle on ajoute une
    # ligne par paire en conflit.  edgelist = c(character(0),character(0));
    edgelist = matrix(c(character(0), character(0)), ncol = 2)  # Créer un tableau du nombre de conflits
    conflictsTotal = as.data.frame(c(character(0)))
    for (i in 1:nrow(tableVariantes)) {
        conflictsTotal = rbind(conflictsTotal, 0)
    }
    row.names(conflictsTotal) = rownames(tableVariantes)  ## na.omit pour ne pas tenir compte des valeurs inconnues.
    ## test[test==0]<-NA pour virer les 0 Convertir toutes les lignes en
    ## factor TEST = apply(tableVariantes,MARGIN=2,FUN=as.factor) #Ne marche
    ## pas très bien.  interaction(TEST[19,],TEST[143,]) [1] 2.1 1.2 2.2 1.1
    ## <NA> 2.2 1.1 <NA> 1.1 1.2 Levels: 1.1 2.1 1.2 2.2
    ## as.factor(TEST[19,]):as.factor(TEST[143,]) [1] 2:1 1:2 2:2 1:1 <NA> 2:2
    ## 1:1 <NA> 1:1 1:2 Levels: 1:1 1:2 2:1 2:2 Optimisation : using variables
    ## to avoid systematic calculation (to do for all the code)
    totalVL = nrow(tableVariantes)
    for (i in 1:(totalVL - 1)) {
        VLA = i  #Renaming it to be more explicit
        supVLA = VLA + 1
        if (!supVLA > totalVL) {
            # Éviter d'avoir un indice hors limites
            for (j in supVLA:totalVL) {
                VLB = j  #Idem
                factorVLA = as.factor(tableVariantes[VLA, ])  #Optimisation : putting it in a factor to avoid repeating this operation
                factorVLB = as.factor(tableVariantes[VLB, ])  #Optimisation : putting it in a factor to avoid repeating this operation
                problematicConfiguration = FALSE  #False until proven otherwise
                interactions = interaction(factorVLA:factorVLB, drop = TRUE)  ##### Ajout de la possibilité d'avoir plusieurs variantes pour un même VL/MS #####     
                if (length(grep(",", interactions)) > 1) {
                  toBeSplitted = grep(".*,.*", interactions, value = TRUE)  #Catching the places where there are several readings
                  interactions = as.factor(grep(".*,.*", interactions, value = TRUE, 
                    invert = TRUE))  #Removing them from the list of interactions
                  factorVLA = as.factor(grep(".*,.*", factorVLA, value = TRUE, 
                    invert = TRUE))  #idem
                  factorVLB = as.factor(grep(".*,.*", factorVLA, value = TRUE, 
                    invert = TRUE))  #idem
                  toBeSplitted = strsplit(toBeSplitted, ":")
                  for (z in toBeSplitted) {
                    N1 = strsplit(z[1], ",")
                    N2 = strsplit(z[2], ",")
                    combinations = expand.grid(N1[[1]], N2[[1]])
                    factorVLA = factor(c(as.character(factorVLA), as.character(combinations[, 
                      1])))
                    factorVLB = factor(c(as.character(factorVLB), as.character(combinations[, 
                      2])))
                    newInteractions = interaction(combinations, sep = ":")
                    interactions = factor(c(as.character(interactions), as.character(newInteractions)))
                  }
                }
                ##### Fin de cet ajout ##### Attention, plus de labels des mss, et plus de
                ##### correspondances en factorVLA et factorVLB (ce qui n'est pas grave vu
                ##### qu'on a interactions déjà, mais s'en souvenir tout de même)
                levelsInteractions = levels(interactions)
                if (length(levelsInteractions) > 3) {
                  # More than three configurations, let's go
                  levelsVLA = levels(factorVLA)  #Variable for optimisation
                  lengthLevelsVLA = length(levelsVLA)
                  testCounter = 0
                  while (problematicConfiguration == FALSE && testCounter < 
                    (lengthLevelsVLA - 1)) {
                    # Stops if one of the conditions is met Testing every level of the first
                    # variant location Let's add a loop to avoid unnecessary comparisons
                    for (k in levelsVLA) {
                      searchForK = paste(k, ":(.+)", sep = "")  # We create the regexp we will need afterwards. Impossible to use variables inside a regex... such a bother...
                      if (length(grep(searchForK, levelsInteractions, ignore.case = TRUE, 
                        perl = TRUE)) > 1) {
                        # More than one configuration for this , here we go Let's start by
                        # capturing the second numbers. Not so easy here.
                        secondNumbers = regexec(searchForK, levelsInteractions, 
                          ignore.case = TRUE)
                        secondNumbers = do.call(rbind, lapply(regmatches(levelsInteractions, 
                          secondNumbers), `[`))  #On reverse les matches dans une variable. Les second nombres sont dans matches[,2]
                        levelsSecondNumbers = levels(as.factor(secondNumbers[, 
                          2]))
                        invertMatches = grep(searchForK, levelsInteractions, 
                          ignore.case = TRUE, perl = TRUE, value = TRUE, 
                          invert = TRUE)  #On récupère les cas où le premier nombre n'est pas celui qu'on est en train de tester.
                        # On crée une liste de ces premiers nombres autres
                        alternativeFirstNumbers = regexec("(.+?):.+", levels(as.factor(invertMatches)), 
                          ignore.case = TRUE)
                        alternativeFirstNumbers = do.call(rbind, lapply(regmatches(levels(as.factor(invertMatches)), 
                          alternativeFirstNumbers), `[`))
                        levelsAlternativeFirstNumbers = levels(as.factor(alternativeFirstNumbers[, 
                          2]))
                        for (l in levelsAlternativeFirstNumbers) {
                          # testing each alternative first number
                          problems = 0  #variable problems to count the problematic configurations #l'emplacement de la définition de cette variable est problématique
                          for (m in levelsSecondNumbers) {
                            # with each alternative second number
                            searchForProblem = paste(l, ":", m, sep = "")
                            if (length(grep(searchForProblem, invertMatches, 
                              ignore.case = TRUE, perl = TRUE)) > 0) {
                              problems = problems + 1
                            }
                            if (problems > 1) {
                              problematicConfiguration = TRUE
                            }
                          }
                        }
                      }
                      testCounter = testCounter + 1
                    }
                  }
                }
                if (problematicConfiguration == TRUE) {
                  edgelist = rbind(edgelist, c(rownames(tableVariantes)[VLA], 
                    rownames(tableVariantes)[VLB]))  #adding a new edge to the network of conflicts !
                  conflictsTotal[VLA, ] = conflictsTotal[VLA, ] + 1
                  conflictsTotal[VLB, ] = conflictsTotal[VLB, ] + 1  #ajouter le nombre de conflits pour chacun des deux lieux variants
                }
            }  # crossing with a second variant location
        }
    }
    centrality = conflictsTotal  ##Computing the centrality index as described in CC 2013
    ## We have to test first that there actual are conflicts in the database
    if (sum(conflictsTotal) > 0) {
        sumConflicts = sum(conflictsTotal)/2
        for (z in 1:nrow(centrality)) {
            # Another test, to avoid division by zero (perhaps the computation of the
            # centrality index should be adapted. Discuss this with Florian. Or, we
            # could accept to have infinite numbers... does it makes sense ? They
            # sure are superior to any centrality threshold we could choose... if()
            centrality[z, ] = centrality[z, ]/(sumConflicts - centrality[z, 
                ])  # added an option to remove infinity and to replace it with 2
            # TODO(global): This is not very clean, nor mentioned in the paper, we
            # should arbitrate on it, and mention it somewhere.
            if (is.infinite(centrality[z, ])) {
                centrality[z, ] = 2
            }
        }
    } else {
        for (z in 1:nrow(centrality)) {
            centrality[z, ] = centrality[z, ] = 0
        }
    }
    ## Now we create the objects that the function will return (a list
    ## containing edgelist, conflictsTotal and original database
    X = NULL
    X = as.list(X)
    X$edgelist = edgelist
    X$conflictsTotal = cbind(conflictsTotal, centrality)
    colnames(X$conflictsTotal) = c("Number of conflicts", "Centrality index")  #X$database = tableVariantes #La base des variantes, donnée en entrée, et dont a besoin pour PCC.contam #Pourrait être remplacé par une option dans la fonction PCC.contam #Il faudrait mieux reprendre x, pour éviter de virer les omissions
    ## Il faudrait insérer ici des commandes pour retirer tous les objets dont
    ## on n'a plus besoin.
    X$database = x  #And now we proceed to draw the plot
    # We have to add a test here, since in some extreme cases, the edgelist
    # can in fact be empty
    if (length(edgelist) != 0) {
        myNetwork = as.network(edgelist, directed = FALSE, matrix.type = "edgelist")  #Important remark here : not specifying matrix.type = edgelist gave, occasionnaly, weird errors, mainly 'Erreur dans abs(x) : argument non numérique pour une fonction mathématique'... So, I am expliciting this option everywhere
        gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork), 
            gmode = "graph", boxed.labels = TRUE)
    } else {
        print("There is absolutely no conflicts in this database.")
    }
    return(X)
}

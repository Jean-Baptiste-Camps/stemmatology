PCC.conflicts <-
    function(x, omissionsAsReadings = FALSE) {
        # Optimisation : temps comparatifs sur données fournival avec system.time()
        # v2 version pré-package (déjà optimisée un peu) - 16/07/2014
        # utilisateur     système      écoulé 
        #      25.030       0.106      25.134 
        # v2.1 version intermédiaire                     - 17/07/2014
        # utilisateur     système      écoulé 
        #      24.774       0.084      24.865 
        # v3 complètement transformée sans regexp (temps d'exécution / par 12 !!!) - 19/07/2014
        # utilisateur     système      écoulé 
        #       2.101       0.004       2.105 
        # Différence total sur PCC.contam:
        # v2
        #utilisateur     système      écoulé 
        #    243.042       0.465     243.471
        # v3
        # utilisateur     système      écoulé 
        #      21.877       0.054      21.926 
        # 
        # TODO(JBC) /!\ very important : vérifier que la nouvelle version donne bien systématiquement les mêmes résultats que l'ancienne sur tous les corpus test. NB: faire aussi cette vérification sur PCC.contam (a priori, devrait fonctionner)
        # TODO(JBC) : important aussi, la nouvelle version, optimisée, ne contient plus la possibilité d'utiliser les virgules pour mettre plusieurs variantes pour le même manuscrit au même lieu variant. Lorsqu'il y en aura besoin, il faudra la réimplémenter. Ce ne devrait pas être si compliqué : il faut juste, lorsqu'une entrée contient des virgules, utiliser strsplit pour séparer les entrées, et utiliser cbind pour ajouter les combinaisons supplémentaires à la liste par ex. si on a, pour A au lieu variant 1 "1,2" et au lieu variant 2 "1", il faut supprimer cette entrée et créer deux nouvelles colonnes donnat 1:1, 2:1
        ## V2 : Nouvelle version pour intégrer la possibilité d'avoir plusieurs
        ## variantes pour le même lieu variant/ms + index de conflictualité
        tableVariantes = as.matrix(x)  #Apparemment le tableau importé est soi disant une liste et non pas un dataframe. Donc, on tente de le contraindre. Normalement, ce ne devrait pas être le cas, si la base a été importée correctement
        tableVariantesInitial = tableVariantes
        if (omissionsAsReadings == FALSE) {
            # Standard case: omissions are not considered as potential common errors
            tableVariantes[tableVariantes == 0] = NA
        }
        # Créer une matrice (edgelist) à deux colonnes, à laquelle on ajoute une
        # ligne par paire en conflit.  edgelist = c(character(0),character(0));
        edgelist = matrix(c(character(0), character(0)), ncol = 2)  # Créer un tableau du nombre de conflits
        # Créer une matrice des conflits (nouvelle solution, optimisée). NB: ceci étant auparavant un dataframe, j'en fait à présent une matrice
        conflictsTotal = matrix(data = 0, nrow = nrow(tableVariantes), ncol = 1, dimnames = list(rownames(tableVariantes), "conflictsTotal")) 
        #### début de la nouvelle version de la comparaison
        for (i in 1:(nrow(tableVariantes) - 1)) {
            # the problematic configuration can only happen if there are at least two unique values for each VL (i and j). So we test for i (if not, we can stop here), and then for j
            # NB: peut-être que d'utiliser des vecteurs au lieu de matrice serait marginalement plus efficace
            if(length(unique(tableVariantes[i, !is.na(tableVariantes[i,]), drop = TRUE])) > 1) {
                # We store the row and its unique values in a vector, because we will need them frequently
                VLA = as.vector(tableVariantes[i, , drop = TRUE])
                uniqueVLA = unique(VLA[!is.na(VLA)])
                for (j in (i + 1):nrow(tableVariantes)) {
                    # Same test for j here
                    if (length(unique(tableVariantes[j, !is.na(tableVariantes[j,]), drop = TRUE])) > 1){
                        VLB = as.vector(tableVariantes[j, , drop = TRUE])
                        # Et c'est ici que les problèmes commencent
                        # We look for configuration such as, given a first , variant location containing {x, x', ..} and a second {y, y', ... }, for each combination {x, y} € m_k and {x, y'} € m_k, is there a x' such as {x', y} € m_k and {x', y'} € m_k
                        problematicConfiguration = FALSE # FALSE until proven otherwise
                        # for each x
                        # and as long as a problematic configuration has not been found
                        for (x in 1:length(uniqueVLA)) {
                            #debug:
                            #print(paste("Now testing uniqueVLA 1", uniqueVLA[x]))
                            #/debug
                            if (problematicConfiguration == TRUE) { break() }
                            # is there at least to y's (omitting NA's) ?
                            if(length(unique(na.omit(VLB[which(VLA == uniqueVLA[x])]))) > 1){
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
                                    ##########PARTIE à revoir
                                    myMatches = vector(mode = "numeric", length = (length(uniqueVLA)))
                                    names(myMatches) = uniqueVLA
                                    # Now for the hard part. We have to see if at least two y have an x' (other than x) in common
                                    for (y in 1:length(uniqueNewVLB)) {
                                        #debug :
                                        #print(paste("iteration number", y, "for uniqueNewVLB", uniqueNewVLB[y]))
                                        #/debug
                                        myNewMatches = unique(na.omit(newVLA[which(newVLB == uniqueNewVLB[y])]))
                                        if( length(myNewMatches) > 0 ){ # Very important test, to verify we are not going to bind a NA column to myMatches...
                                            for (m in 1:length(myNewMatches) ) {
                                                myMatches[as.character(myNewMatches[m])] = myMatches[as.character(myNewMatches[m])] +1  # It might be a tad faster not to use vector names for the entry, which can be done by using myMatches[which(uniqueVLA == myNewMatches[m], arr.ind = TRUE)] => i'll try that after
                                            }
                                        }
                                        if (length(myMatches[myMatches > 1]) > 0) {
                                            problematicConfiguration = TRUE
                                            # Debug :
                                            #print(paste("Problematic configuration found for VL", rownames(tableVariantes)[i], rownames(tableVariantes)[j]))
                                            #print(myMatches)
                                            #print(myNewMatches)
                                            #/debug
                                            break()
                                        }
                                    }
                                }
                            }
                        }
                        if (problematicConfiguration == TRUE) {
                            edgelist = rbind(edgelist, c(rownames(tableVariantes)[i], 
                                                         rownames(tableVariantes)[j]))  #adding a new edge to the network of conflicts !
                            conflictsTotal[i, ] = conflictsTotal[i, ] + 1
                            conflictsTotal[j, ] = conflictsTotal[j, ] + 1  #ajouter le nombre de conflits pour chacun des deux lieux variants
                        }
                    }
                }
            }
            
            #### Fin de la nouvelle version de la comparaison
            #### Début de la v2 de la comparaison
            # for (i in 1:(nrow(tableVariantes) - 1)) {
            #     #VLA = i  #Renaming it to be more explicit
            #     #supVLA = VLA + 1
            #     #if (!supVLA > nrow(tableVariantes)) {
            #     # Éviter d'avoir un indice hors limites #On ne devrait pas en avoir un, car le calcul est déjà fait au dessus !
            #     for (j in (i + 1):nrow(tableVariantes)) {
            #         VLB = j  #Idem
            #         factorVLA = as.factor(tableVariantes[i, ])  #Optimisation : putting it in a factor to avoid repeating this operation
            #         factorVLB = as.factor(tableVariantes[j, ])  #Optimisation : putting it in a factor to avoid repeating this operation
            #         problematicConfiguration = FALSE  #False until proven otherwise
            #         interactions = interaction(factorVLA:factorVLB, drop = TRUE)  ##### Ajout de la possibilité d'avoir plusieurs variantes pour un même VL/MS #####     
            #         if (length(grep(",", interactions)) > 1) {
            #             toBeSplitted = grep(".*,.*", interactions, value = TRUE)  #Catching the places where there are several readings
            #             interactions = as.factor(grep(".*,.*", interactions, value = TRUE, 
            #                                           invert = TRUE))  #Removing them from the list of interactions
            #             factorVLA = as.factor(grep(".*,.*", factorVLA, value = TRUE, 
            #                                        invert = TRUE))  #idem
            #             factorVLB = as.factor(grep(".*,.*", factorVLA, value = TRUE, 
            #                                        invert = TRUE))  #idem
            #             toBeSplitted = strsplit(toBeSplitted, ":")
            #             for (z in toBeSplitted) {
            #                 N1 = strsplit(z[1], ",")
            #                 N2 = strsplit(z[2], ",")
            #                 combinations = expand.grid(N1[[1]], N2[[1]])
            #                 factorVLA = factor(c(as.character(factorVLA), as.character(combinations[, 
            #                                                                                         1])))
            #                 factorVLB = factor(c(as.character(factorVLB), as.character(combinations[, 
            #                                                                                         2])))
            #                 newInteractions = interaction(combinations, sep = ":")
            #                 interactions = factor(c(as.character(interactions), as.character(newInteractions)))
            #             }
            #         }
            #         ##### Fin de cet ajout ##### Attention, plus de labels des mss, et plus de
            #         ##### correspondances en factorVLA et factorVLB (ce qui n'est pas grave vu
            #         ##### qu'on a interactions déjà, mais s'en souvenir tout de même)
            #         levelsInteractions = levels(interactions)
            #         if (length(levelsInteractions) > 3) {
            #             # More than three configurations, let's go
            #             levelsVLA = levels(factorVLA)  #Variable for optimisation
            #             lengthLevelsVLA = length(levelsVLA)
            #             testCounter = 0
            #             while (problematicConfiguration == FALSE && testCounter < 
            #                        (lengthLevelsVLA - 1)) {
            #                 # Stops if one of the conditions is met Testing every level of the first
            #                 # variant location Let's add a loop to avoid unnecessary comparisons
            #                 for (k in levelsVLA) {
            #                     searchForK = paste(k, ":(.+)", sep = "")  # We create the regexp we will need afterwards. Impossible to use variables inside a regex... such a bother...
            #                     if (length(grep(searchForK, levelsInteractions, ignore.case = TRUE, 
            #                                     perl = TRUE)) > 1) {
            #                         # More than one configuration for this , here we go Let's start by
            #                         # capturing the second numbers. Not so easy here.
            #                         secondNumbers = regexec(searchForK, levelsInteractions, 
            #                                                 ignore.case = TRUE)
            #                         secondNumbers = do.call(rbind, lapply(regmatches(levelsInteractions, 
            #                                                                          secondNumbers), `[`))  #On reverse les matches dans une variable. Les second nombres sont dans matches[,2]
            #                         levelsSecondNumbers = levels(as.factor(secondNumbers[, 
            #                                                                              2]))
            #                         invertMatches = grep(searchForK, levelsInteractions, 
            #                                              ignore.case = TRUE, perl = TRUE, value = TRUE, 
            #                                              invert = TRUE)  #On récupère les cas où le premier nombre n'est pas celui qu'on est en train de tester.
            #                         # On crée une liste de ces premiers nombres autres
            #                         alternativeFirstNumbers = regexec("(.+?):.+", levels(as.factor(invertMatches)), 
            #                                                           ignore.case = TRUE)
            #                         alternativeFirstNumbers = do.call(rbind, lapply(regmatches(levels(as.factor(invertMatches)), 
            #                                                                                    alternativeFirstNumbers), `[`))
            #                         levelsAlternativeFirstNumbers = levels(as.factor(alternativeFirstNumbers[, 
            #                                                                                                  2]))
            #                         for (l in levelsAlternativeFirstNumbers) {
            #                             # testing each alternative first number
            #                             problems = 0  #variable problems to count the problematic configurations #l'emplacement de la définition de cette variable est problématique
            #                             for (m in levelsSecondNumbers) {
            #                                 # with each alternative second number
            #                                 searchForProblem = paste(l, ":", m, sep = "")
            #                                 if (length(grep(searchForProblem, invertMatches, 
            #                                                 ignore.case = TRUE, perl = TRUE)) > 0) {
            #                                     problems = problems + 1
            #                                 }
            #                                 if (problems > 1) {
            #                                     problematicConfiguration = TRUE
            #                                 }
            #                             }
            #                         }
            #                     }
            #                     testCounter = testCounter + 1
            #                 }
            #             }
            #         }
            #             if (problematicConfiguration == TRUE) {
            #                 edgelist = rbind(edgelist, c(rownames(tableVariantes)[i], 
            #                                              rownames(tableVariantes)[j]))  #adding a new edge to the network of conflicts !
            #                 conflictsTotal[i, ] = conflictsTotal[i, ] + 1
            #                 conflictsTotal[j, ] = conflictsTotal[j, ] + 1  #ajouter le nombre de conflits pour chacun des deux lieux variants
            #             }
            ##### fin de la v2 de la comparaison
        }  # end of crossing with a second variant location
        #} fermeture du if (!supVLA > nrow(tableVariantes)) {
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
            myNetwork = as.network(edgelist, directed = FALSE, matrix.type = "edgelist")  #Important remark here : not specifying matrix.type = edgelist gave, occasionnaly, weird errors, mainly 'Erreur dans abs(x) : argument non numérique pour une fonction mathématique'... So, I am expliciting this option everywhere
            gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork), 
                  gmode = "graph", boxed.labels = TRUE)
        } else {
            print("There is absolutely no conflicts in this database.")
        }
        return(output)
    }
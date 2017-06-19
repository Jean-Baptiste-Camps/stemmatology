PCC.overconflicting<-
    function(x) {
        ##### Fonction d'élimination des lieux variants problématiques, avec quelques
        ##### aides au choix. En entrée, un object de type PCC.conflicts Finitions :
        ##### il reste encore à intégrer le partitioning around medoids et sa
        ##### représentation sur le barplot. On pourrait se contenter d'utiliser la
        ##### fonction de colorisation des barres, à partir des classes données par
        ##### le pam. #EDIT 19 juin 2017: il me semble que c'est fait à présent.
        reseau = x
        ordConflTot = reseau$conflictsTotal[order(reseau$conflictsTotal[, 1], 
                                                  decreasing = TRUE), ]  
        # On commence ici par quelques fonctions graphiques (barplots) pour aider
        # au choix du seuil de centralité, avec en renfort un clustering de type
        # partitioning around medoids (k-means en plus robuste et meilleur) avec
        # 4 classes, et en colorant les barres selon ces classes
        # NB: pour l'instant on utilise essentiellement les valeurs par défaut de
        # cette fonction, mais il y en a d'autres (euclid ou manhattan, ...).
        # Mais a priori, d'après les tests effectués, les données sont
        # suffisamment marquées et la méthode robuste pour que ça ne change
        # strictement rien. Just test before that there is more than three
        # individuals in the database. If not, warn the user and use three
        # classes only
        testClasses = ordConflTot[, 1]
        if (length(testClasses[testClasses > 0]) > 3) {
            numberOfClasses = 4
        } else {
            if (length(testClasses[testClasses > 0]) > 1) {
                print("The number of conflicts is VERY LOW. Is your database correct?")
                numberOfClasses = 3
            }
            if (length(testClasses[testClasses > 0]) == 0) {
                stop("There is no conflicts in this database.")
            }
        }
        classes1 = pam(ordConflTot[,1], numberOfClasses)
        barplot(ordConflTot[, 1], col = classes1$clustering, main = "Total de conflits par lieu variant", 
                names.arg = rownames(ordConflTot), xlab = "VL", ylab = "Conflicts Total", 
                ylim = (c(0, ordConflTot[1, 1])), cex.axis = "1", sub = "coloured according to partitioning around medoids with 4 clusters", 
                yaxt = "n")
        axis(side = 2, at = seq(0, ordConflTot[1, 1], by = 2))  
        #Arrêt, et demander à procéder jusqu'au second graphique
        par(ask = TRUE)
        classes2 = pam(ordConflTot[, 2], numberOfClasses)
        barplot(ordConflTot[, 2], col = classes2$clustering, main = "Indice de centralité par lieu variant", 
                names.arg = rownames(ordConflTot), xlab = "VL", ylab = "Centrality Index", 
                ylim = (c(0, ordConflTot[1, 2])), cex.axis = "1", sub = "coloured according to partitioning around medoids with 4 clusters", 
                yaxt = "n")
        axis(side = 2, at = seq(0, ordConflTot[1, 2], by = 0.02))  
        ## On présente les degrés de centralité visuellement. Réfléchir si d'autres
        ## méthodes pourraient être possibles.
        ## Serait intéressant de voir si on peut clusterer pour voir ou couper, ou
        ## faire des suggestions à l'utilisateur Voir si on peut aller du côté des
        ## k-means, scale, et autres. 
        # Pour l'instant, fonction pam du package cluster Partitioning Around Medoids
        ## Description: Partitioning (clustering) of the data into ‘k’ clusters
        ## ``around medoids'', a more robust version of K-means. Marche vraiment
        ## très bien avec 4 classes, du moins sur l'échantillon Chevalier au Lyon
        ## (assez logique : une classe pour les over-conflicting, une pour les non
        ## assessables, une pour les sobres, et une pour les non-conflicting) Avec
        ## 3 classes, marche vraiment très bien aussi, dans le sens où les deux
        ## premiers groupes s'assemblent Éventuellement, voir aussi du côté du
        ## module ClustOfVar (http://arxiv.org/pdf/1112.0295v1.pdf), assez
        ## intéressant (mais Commençons par créer le réseau
        myNetwork = as.network(reseau$edgelist, directed = FALSE, matrix.type = "edgelist")  
        #Définissons un seuil de centralité
        answered = FALSE
        while (answered == FALSE) {
            seuilCentrality = as.numeric(readline("Choisissez le seuil de centralité > "))
            if (is.na(seuilCentrality)) {
                print("Please enter a number.")
            } else {
                if (seuilCentrality >= 2) {
                    print("Please enter a number inferior to 2 (which is the maximum possible value).")
                } else {
                    answered = TRUE
                }
            }
        }
        # pour éviter d'utiliser des facteurs qu'on ne voudrait pas
        options(stringsAsFactors = FALSE) 
        # On crée une table d'étiquettes pour les nœuds (vertexAttributes)
        hasConflicts = reseau$conflictsTotal[reseau$conflictsTotal[, 2] > 0, , drop = FALSE]
        vertexAttributes = matrix(nrow = nrow(hasConflicts), ncol = 2, dimnames = list(rownames(hasConflicts), c("label","color")))
        # Et on étiquette les nœuds, selon le seuil de centralité
        vertexAttributes[hasConflicts[, 2] > seuilCentrality, 1] = "overconflicting"
        vertexAttributes[hasConflicts[, 2] > seuilCentrality, 2] = "red"
        vertexAttributes[hasConflicts[, 2] < seuilCentrality & hasConflicts[, 2] > 0, 1] = "unknown"
        vertexAttributes[hasConflicts[, 2] < seuilCentrality & hasConflicts[, 2] > 0, 2] = "grey"
        # Voyons maintenant pour étiqueter les «sobres» :
        # On récupère la table d'adjacence
        adjacencyTable = as.matrix.network(myNetwork, matrix.type = "adjacency")
        # et on boucle sur les nœuds
        for (i in 1:nrow(vertexAttributes)) {
            # Si le nœud n'est pas lui-même overconflicting
            if (vertexAttributes[i, 1] != "overconflicting") {
                # undecidable = 0  #J'enlève ça pour l'instant, mais on pourrait
                # vouloir à terme compter le nombre de conflits avec des non
                # overconflicting
                undecidable = FALSE
                # On croise contre toutes les colonnes de la matrice d'adjacence
                for (j in 1:ncol(adjacencyTable)) {
                    # S'il y a un lien
                    if (adjacencyTable[rownames(vertexAttributes)[i], j] > 0) {
                        # on teste pour voir si chacun des nœuds avec lesquels il y a lien sont
                        # eux-mêmes overconflicting
                        if (vertexAttributes[colnames(adjacencyTable)[j], 1] != 
                                "overconflicting") {
                            # si non, alors le nœud est undécidable
                            undecidable = TRUE
                            break()
                        }
                    }
                }
                if (undecidable == FALSE) {
                    vertexAttributes[i, ] = c("sober", "green")
                }
            }
        }
        # À présent, on va attribuer aux nœuds du réseau les attributs de
        # couleur. Pour ce faire, on doit récupérer les identifiants à partir des
        # étiquettes.
        vertexNameId = network.vertex.names(myNetwork)
        vertexId = NULL
        for (i in 1:nrow(vertexAttributes)) {
            vertexId = c(vertexId, which(vertexNameId == rownames(vertexAttributes)[i]))
        }
        # Et maintenant, on les utilise pour modifier les nœuds
        myNetwork = set.vertex.attribute(myNetwork, "color", vertexAttributes[ , "color"],
                                         v = vertexId)
        gplot(myNetwork, displaylabels, label = network.vertex.names(myNetwork), 
              gmode = "graph", vertex.col = get.vertex.attribute(myNetwork, "color"), 
              boxed.labels = TRUE)
        par(ask = FALSE) 
        # we add the vertexAttributes to the pccConflicts object inputed, and
        # return it as a pccElimination object
        reseau$vertexAttributes = vertexAttributes 
        class(reseau) = "pccElimination"
        return(reseau)
    }

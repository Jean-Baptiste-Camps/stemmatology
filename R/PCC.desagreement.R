PCC.desagreement <-
function(x, omissionsAsReadings = FALSE) {
    # En entrée une table de variantes. En sortie, les matrices de désaccords
    # et la base de données in supplement of the tables giving benigne and
    # severe desagreements, the function also returns, for information only,
    # the count of agreements, common omissions,
    tableVariantes = as.matrix(x)  #Should we use here data.matrix to coerce to a data matrix the original database? => to do, as long as we do not integrate the ability to have several readings for a same place.
    # tableVariantes = data.matrix(x) #On crée des tables proposant un
    # croisement de l'ensemble des manuscrits D'abord, celles qui servent à
    # la construction du stemma N.B.: à terme, pour gérer les cas où un
    # manuscrit à plusieurs leçons à un lieu variant donné (ou a plusieurs
    # leçons à partir d'un traitement de la contamination), il faudra
    # peut-être faire une table de ce type pour chaque lieu variant, et
    # calculer les sommes de manière différenciées à la fin.
    severeDesagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    benigneDesagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))  #Ensuite, celles qui sont proposées à titre informatif
    agreements = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsInCommon = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsOriented = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))  #Je n'en mets pas pour les NA, qui, en tant que manque d'information, peuvent être soit des lacunes, soit des oublis d'encodage, soit des oublis de collation, etc.
    # On prend chacune des lignes (= lieux variants)
    for (i in 1:nrow(tableVariantes)) {
        # Puis, on prend chacune des colonnes (= manuscrits)
        for (j in 1:ncol(tableVariantes)) {
            # Premier cas, la leçon du ms. est inconnue, donc on ne fait rien
            if (is.na(tableVariantes[i, j])) {
            } else {
                # Si la leçon est connue, on croise avec tous les autres Si on veut, on
                # peut éviter de croiser avec soi-même le ms. grâce à la commande
                # variantesAComp = tableVariantes[i,-j] #####Début du traitement des
                # omissions Commençons par tester si la leçon est une omission ou non
                if ((tableVariantes[i, j] == 0) && (omissionsAsReadings == 
                  FALSE)) {
                  for (k in 1:ncol(tableVariantes)) {
                    # On teste d'abord que l'on ne croise pas avec une valeur manquante
                    if (is.na(tableVariantes[i, k]) == FALSE) {
                      # Now we know that there is at least a case where an omission in one of
                      # the witness as been compared to the other, so we set the two values to
                      # 0 if they were NA
                      if (is.na(omissionsInCommon[colnames(tableVariantes)[j], 
                        colnames(tableVariantes)[k]])) {
                        omissionsInCommon[colnames(tableVariantes)[j], colnames(tableVariantes)[k]] = 0
                      }
                      if (is.na(omissionsOriented[colnames(tableVariantes)[j], 
                        colnames(tableVariantes)[k]])) {
                        omissionsOriented[colnames(tableVariantes)[j], colnames(tableVariantes)[k]] = 0
                      }
                      # Premier cas, omission commune
                      if (tableVariantes[i, j] == tableVariantes[i, k]) {
                        # On utilise la fonction maison pour incrémenter la table
                        omissionsInCommon = incrementTable(omissionsInCommon, 
                          tableVariantes, j, k)  #if(is.na(omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]])){
                        # omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # = 1 #}
                        # else{omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # =
                        # omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # + 1 ; }
                      }
                      # Second cas, omission orientée
                      if (tableVariantes[i, j] < tableVariantes[i, k]) {
                        omissionsOriented = incrementTable(omissionsOriented, 
                          tableVariantes, j, k)  #if(is.na(omissionsOriented[colnames(tableVariantes)[j],colnames(tableVariantes)[k]])){
                        # omissionsOriented[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # = 1 #}
                        # else{omissionsOriented[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # =
                        # omissionsOriented[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # + 1 ; }
                      }
                    }
                  }
                } else {
                  #### Début du traitement des leçons Maintenant, nous savons qu'elle est une
                  #### leçon, mais faisons une dernière vérification
                  if (is.numeric(tableVariantes[i, j])) {
                    # Maintenant, everything is good for a go
                    for (k in 1:ncol(tableVariantes)) {
                      # si k n'est pas NA, et que k n'est pas égal à 0 ou que
                      # omissionsAsReadings == TRUE
                      if ((is.na(tableVariantes[i, k]) == FALSE) && ((tableVariantes[i, 
                        k] != 0) | (omissionsAsReadings == TRUE))) {
                        # Careful, for this is a bit tricky : before comparing the two readings,
                        # we already have established something: the two manuscripts can be
                        # compared in one occasion. This means that the value for the comparison,
                        # if it is NA at this point, should not be. That's why we test for NA and
                        # set to 0.
                        if (is.na(agreements[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          agreements[colnames(tableVariantes)[j], colnames(tableVariantes)[k]] = 0
                        }
                        if (is.na(severeDesagreement[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          severeDesagreement[colnames(tableVariantes)[j], 
                            colnames(tableVariantes)[k]] = 0
                        }
                        if (is.na(benigneDesagreement[colnames(tableVariantes)[j], 
                          colnames(tableVariantes)[k]])) {
                          benigneDesagreement[colnames(tableVariantes)[j], 
                            colnames(tableVariantes)[k]] = 0
                        }
                        # Now that it is done, let's proceed to actual comparison Premier cas, il
                        # y a accord if j = k, alors agreements++
                        if (tableVariantes[i, j] == tableVariantes[i, k]) {
                          agreements = incrementTable(agreements, tableVariantes, 
                            j, k)
                        }
                        # if j != k alors
                        if (tableVariantes[i, j] != tableVariantes[i, k]) {
                          # si j existe ailleurs dans la ligne et k existe ailleurs dans la ligne,
                          # severeDisagreements++ On commence donc par créer une table des autres
                          # variantes de ce même lieu (i.e. tous sauf j et k)
                          tableSansjk = tableVariantes[i, -c(j, k), drop = FALSE]  #Puis on croise
                          # x%in%y permet de tester la présence de la valeur x dans le vecteur j,
                          # et retourne TRUE ou FALSE ; les fonctions any(y==x) permet de faire
                          # exactement la même chose, tout comme is.element(x,y); pour récupérer la
                          # première occurrence de x dans y, on peut utiliser match(x,y) ou
                          # which(x,y)
                          if ((tableVariantes[i, j] %in% tableSansjk) && 
                            (tableVariantes[i, k] %in% tableSansjk)) {
                            severeDesagreement = incrementTable(severeDesagreement, 
                              tableVariantes, j, k)
                          } else {
                            # sinon benigneDesagreements++
                            benigneDesagreement = incrementTable(benigneDesagreement, 
                              tableVariantes, j, k)
                          }
                        }
                      }
                    }
                  } else {
                    errorMessage = paste("You have non numeric values in the database at [", 
                      i, ",", j, "] aborting calculation.")
                    print(errorMessage)
                    break
                  }
                }
            }
        }
    }
    X = character(0)
    X = as.list(X)
    X$database = tableVariantes
    X$severeDesagreement = severeDesagreement
    X$benigneDesagreement = benigneDesagreement  #Ensuite, celles qui sont proposées à titre informatif
    X$agreements = agreements
    X$omissionsInCommon = omissionsInCommon
    X$omissionsOriented = omissionsOriented
    return(X)
}

PCC.desagreement <-
function(x, omissionsAsReadings = FALSE) {
    # Input are tables of variants. Ouput are desagreement matrices and database.
    # In addition to the tables giving benigne and severe
    # desagreements, the function also returns, for information only,
    # the count of agreements and common omissions,
    tableVariantes = as.matrix(x)  
    
    if (is.matrix(x) = FALSE)  {
        stop ("Input is not a matrix.")
    } 
    
    # tableVariantes = data.matrix(x) #We create tables proposing to cross
    # the whole set of manuscripts. First, celles qui servent to
    # build the stemma N.B.: Plus tard, pour gerer les cas d un
    # manuscrit a plusieurs lecons a un lieu variant donne (ou a plusieurs
    # leçons à partir d'un traitement de la contamination), il faudra
    # peut-être faire une table de ce type pour chaque lieu variant, et
    # calculer les sommes de maniere differenciees a la fin.
    severeDesagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    benigneDesagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))  #Then the one being proposed as an FYI
    agreements = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsInCommon = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))
    omissionsOriented = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
        dimnames = c(labels(tableVariantes)[2], labels(tableVariantes)[2]))  #Je n'en mets pas pour les NA, qui, en tant que manque d'information, peuvent être soit des lacunes, soit des oublis d'encodage, soit des oublis de collation, etc.
    # We take each line (= variant locations)
    for (i in 1:nrow(tableVariantes)) {
        # Then we take each column (= manuscritps)
        for (j in 1:ncol(tableVariantes)) {
            # First case: the lesson of tha manuscript is unknown, and we don't do anything
            if (is.na(tableVariantes[i, j])) {
            } else {
                # If the lesson is known, on croise avec tous les autres Si on veut, on
                # peut éviter de croiser avec soi-même le ms. thanks to la commande
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
                      # First case: common omission
                      if (tableVariantes[i, j] == tableVariantes[i, k]) {
                        # We use our own function for the table's incrementation.
                        omissionsInCommon = incrementTable(omissionsInCommon, 
                          tableVariantes, j, k)  #if(is.na(omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]])){
                        # omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # = 1 #}
                        # else{omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # =
                        # omissionsInCommon[colnames(tableVariantes)[j],colnames(tableVariantes)[k]]
                        # + 1 ; }
                      }
                      # Second case: oriented omission
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
                  #### Beginning of lesson's treatment. Maintenant, nous savons qu'elle est une
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
                          # x%in%y Allows to test the presence of value x in vector j,
                          # and returns TRUE ou FALSE. The functions any(y==x) allow to do 
                          # the exact same thing, as is.element(x,y); to get the first occurence of
                          #  x in y, we can use match(x,y) or
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
    X$benigneDesagreement = benigneDesagreement  #Then the one being proposed as an FYI.
    X$agreements = agreements
    X$omissionsInCommon = omissionsInCommon
    X$omissionsOriented = omissionsOriented
    return(X)
}

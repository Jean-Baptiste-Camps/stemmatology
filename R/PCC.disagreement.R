PCC.disagreement <-
    function(x, omissionsAsReadings = FALSE) {
        # As input, a variants table (numeric matrix). Ouput are disagreement 
        # matrices and database. In addition to the tables giving benign and severe
        # disagreements, common and oriented omissions, the function also returns,
        # for information, the count of agreements between manuscripts.
        tableVariantes = as.matrix(x)
        if (is.matrix(x) == FALSE || is.numeric(x) == FALSE)  {
            stop ("Input is not a numeric matrix.")
        } 
        # tableVariantes = data.matrix(x) 
        #We create tables crossing the whole set
        # of manuscripts. First, the one useful to build the stemma
        myWit = dimnames(tableVariantes)[[2]]
        myDimnames = list(myWit,myWit)
        
        severeDisagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
                                    dimnames = myDimnames)
        benignDisagreement = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
                                     dimnames = myDimnames)  
        omissionsInCommon = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
                                   dimnames = myDimnames)
        omissionsOriented = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
                                   dimnames = myDimnames) 
        #Then the one being proposed as an FYI
        agreements = matrix(nrow = ncol(tableVariantes), ncol = ncol(tableVariantes), 
                            dimnames = myDimnames)
        rm(myDimnames)
        # Je n'en mets pas pour les NA, qui, en tant que manque d'information, 
        # peuvent être soit des lacunes, soit des oublis d'encodage, soit des 
        # oublis de collation, etc. 
        # We take each line (= variant location)
        for (i in 1:nrow(tableVariantes)) {
            # Then we take each column (= manuscripts)
            for (j in 1:ncol(tableVariantes)) {
                # First case: the lesson of the manuscript is unknown, and we don't do anything
                if (is.na(tableVariantes[i, j])) {
                } else {
                  myWitJ = myWit[j]
                    #If the lesson is known, we compare it to every known lesson.
                    ## Let's start by handling the omissions.
                    if ((tableVariantes[i, j] == 0) && (omissionsAsReadings == 
                                                            FALSE)) {
                        for (k in 1:ncol(tableVariantes)) {
                          myWitK = myWit[k]
                            # On teste d'abord que l'on ne croise pas avec une
                            # valeur manquante
                            if (is.na(tableVariantes[i, k]) == FALSE) {
                                # Now we know that there is at least a case
                                # where an omission in one of the witness has
                                # been compared to the other, so we set the two
                                # values to 0 if they were NA
                                if (is.na(omissionsInCommon[myWitJ, 
                                                            myWitK])) {
                                    omissionsInCommon[myWitJ, myWitK] = 0
                                }
                                if (is.na(omissionsOriented[myWitJ, 
                                                            myWitK])) {
                                    omissionsOriented[myWitJ, myWitK] = 0
                                }
                                # First case: common omission
                                if (tableVariantes[i, j] == tableVariantes[i, k]) {
                                    # We use our own function for the table's incrementation.
                                    omissionsInCommon[myWitJ,myWitK] = omissionsInCommon[myWitJ,myWitK] + 1
                                }
                                # Second case: oriented omission
                                if (tableVariantes[i, j] < tableVariantes[i, k]) {
                                    omissionsOriented[myWitJ,myWitK] = omissionsOriented[myWitJ,myWitK] +1
                                }
                            }
                        }
                    } else {
                        #### Beginning of lesson's treatment.
                        for (k in 1:ncol(tableVariantes)) {
                            # if k is nor NA, nor equal to 0
                            # ou que omissionsAsReadings == TRUE
                          myWitK = myWit[k]
                            if ((is.na(tableVariantes[i, k]) == FALSE) && ((tableVariantes[i, k] != 0) || (omissionsAsReadings == TRUE))) {
                                # Careful, for this is a bit tricky : before
                                # comparing the two readings, we already
                                # have established something: the two
                                # manuscripts can be compared in one
                                # occasion. This means that the value for
                                # the comparison, if it is NA at this point,
                                # should not be. That's why we test for NA
                                # and set to 0.
                                # NB: this is where we remove NA, that will subsist only if
                                # the category examined (agreements, disagreements, etc.)
                                # between two manuscripts is completely empty.
                                if (is.na(agreements[myWitJ, 
                                                     myWitK])) {
                                    agreements[myWitJ, myWitK] = 0
                                }
                                if (is.na(severeDisagreement[myWitJ, 
                                                             myWitK])) {
                                    severeDisagreement[myWitJ, 
                                                       myWitK] = 0
                                }
                                if (is.na(benignDisagreement[myWitJ, 
                                                              myWitK])) {
                                    benignDisagreement[myWitJ, 
                                                        myWitK] = 0
                                }
                                # Now that it is done, let's proceed to
                                # actual comparison. First case, il y a
                                # accord if j = k, alors agreements++
                                if (tableVariantes[i, j] == tableVariantes[i, k]) {
                                    agreements[myWitJ,myWitK] = agreements[myWitJ,myWitK] + 1 
                                }
                                # if j != k alors
                                if (tableVariantes[i, j] != tableVariantes[i, k]) {
                                    # si j existe ailleurs dans la ligne et 
                                    # k existe ailleurs dans la ligne, 
                                    # severeDisagreements++ On commence donc
                                    # par créer une table des autres 
                                    # variantes de ce même lieu (i.e. tous 
                                    # sauf j et k)
                                    tableSansjk = tableVariantes[i, -c(j, k), drop = FALSE] 
                                    #Puis on croise. x%in%y Allows to test
                                    #the presence of value x in vector j, 
                                    #and returns TRUE ou FALSE. The
                                    #functions any(y==x) allow to do the
                                    #exact same thing, as is.element(x,y);
                                    #to get the first occurence of x in y,
                                    #we can use match(x,y) or
                                    # which(x,y)
                                    if ((tableVariantes[i, j] %in% tableSansjk) && 
                                            (tableVariantes[i, k] %in% tableSansjk)) {
                                        severeDisagreement[myWitJ,myWitK] = severeDisagreement[myWitJ,myWitK] +1
                                    } else {
                                        # sinon benignDisagreements++
                                        benignDisagreement[myWitJ,myWitK] = benignDisagreement[myWitJ,myWitK] +1
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        X = character(0)
        X = as.list(X)
        X$database = tableVariantes
        X$severeDisagreement = severeDisagreement
        X$benignDisagreement = benignDisagreement  #Then the one being proposed as an FYI.
        X$agreements = agreements
        X$omissionsInCommon = omissionsInCommon
        X$omissionsOriented = omissionsOriented
        return(X)
    }

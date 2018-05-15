PCC.distribute <- 
    function(x){
        ### Function that takes in entry a character matrix, with values 
        ### containing comma separated alternate readings, and that 
        ### «dealternates» them by distribution in new variant locations. 
        ### /!\ This can quite increase the size of the database, but it 
        ### will most likely speed up further calculations as well
        ### TODO(JBC): check the results and confront it on PCC.conflicts 
        ### with the treatment of a undistributed DB
        ### NB: dans l'esprit, plutôt une fonction à appliquer sur une base
        ### déjà nettoyée ?
        if (!is.matrix(x) && !is.character(x)) {
            stop("The imput database is not a character matrix. If it is a numeric matrix (i.e. not containing alternate readings), this function is unnecessary.")
        }
        tableVariantes = as.matrix(x)
        # We create a list of rows to be removed, as well as a list of the new rows
        toBeRemoved = NULL
        myNewRows = list(NULL)
        myNewRownames = NULL
        for (i in 1:nrow(tableVariantes)) {
            # first, we test to see if there are some alternate readings in the row
            myAlternates = grep(",", tableVariantes[i,], fixed = TRUE)
            if (length(myAlternates) > 0) {
                # the row will be removed and replaced with the new distributed rows
                toBeRemoved = c(toBeRemoved, i)
                # we turn the row in a list
                myRow = as.list(tableVariantes[i,])
                # we split the rows containing ","
                for (j in 1:length(myAlternates)) {
                    myRow[myAlternates[j]] = strsplit( myRow[[myAlternates[j]]],
                                                       split = ",", fixed = TRUE)  
                }
                # we create the new rows
                rowsToAdd = expand.grid(myRow, stringsAsFactors = FALSE)
                # we create new rownames by concatenating the rowname with a unique number by new row
                myNewRownames = c(myNewRownames, paste(rownames(tableVariantes)[i], "-", 1:nrow(rowsToAdd), sep = ""))
                myNewRows = mapply(c, myNewRows, rowsToAdd, SIMPLIFY = FALSE)
                
                # rbind will perhaps be cleared, but slower as well.
            }
        }
        if(!is.null(toBeRemoved) && !is.null(myNewRows)) {
            tableVariantes = tableVariantes[-toBeRemoved, , drop = FALSE]
            tableVariantes = rbind(tableVariantes,  matrix(unlist(myNewRows), ncol = ncol(tableVariantes), dimnames = list(myNewRownames, NULL)))
            mode(tableVariantes) = "numeric"
        }
        return(tableVariantes)
    }

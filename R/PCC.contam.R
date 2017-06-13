PCC.contam <-
function(x, pauseAtPlot = FALSE, omissionsAsReadings = FALSE, alternateReadings = FALSE) {
    ##### Fonction supplémentaire testant le retrait de chacun des mss pour voir
    ##### la différence en termes de nombre de conflits ### /!\ temps très long
    ##### d'exécution... Pour Ambroise, a priori 290s*16 soit 1h20 # En entrée,
    ##### un objet de type PCC.conflicts Various contamination assessment
    ##### methods. /!\ Long execution time.  05/07/2014: added an option to
    ##### pause at each plot
    if (pauseAtPlot == TRUE) {
        par(ask = TRUE)
    } else {
        par(ask = FALSE)
    }
    if (is.matrix(x)) {
        tableVariantes = x
    } else {
        if (class(x) == "pccConflicts" | class(x) == "pccElimination") { # TODO(JBC): There is a cleaner way to define methods for classes
            tableVariantes = x$database
        } else {
            if (class(x) == "pccEquipollentDatabases") {
                stop("It does not really make sense to apply PCC.contam() to an already equipollented database.")
            } else {
                stop("Input is neither a matrix, nor a object of class pccConflicts or pccElimination.")
            }
        }
    }
    #conflictsDifferences = as.data.frame(character(0))
    conflictsDifferences = as.matrix(integer(0))
    X = as.list(NULL)
    X$totalByMs = x$conflictsTotal
    for (i in 1:ncol(tableVariantes)) {
        database = tableVariantes[, -i, drop = FALSE]  #Adding the new row to the synthesis of the conflicts differences
        pccConflicts = PCC.conflicts(database, omissionsAsReadings = omissionsAsReadings, alternateReadings = alternateReadings)  #Adding a label to the plot (if there is actually a plot, otherwise, stating that there is no conflicts
        if (length(pccConflicts$edgelist) != 0) {
            legend("topright", c("Without ms. ", colnames(tableVariantes)[i]))
        } else {
            message = paste("Without ms. ", colnames(tableVariantes)[i], 
                "there is NO CONFLICTS in the database. If the number of conflicts is \n otherwise high, it is likely that this manuscript is in cause")
            writeLines(message)
            if (pauseAtPlot == TRUE) {
                cat("Press [enter] to continue")
                line = readline()
            }
        }
        # Calculating the differences in conflict number
        difference = (sum(pccConflicts$conflictsTotal[,1])/2) - (sum(x$conflictsTotal[,1])/2)
        conflictsDifferences = rbind(conflictsDifferences, difference)
        rownames(conflictsDifferences)[i] = colnames(tableVariantes)[i]  #Adding the conflicts per VL with this ms. removed
        colnames(pccConflicts$conflictsTotal)[1] = colnames(tableVariantes)[i]
        X$totalByMs = cbind(X$totalByMs, pccConflicts$conflictsTotal)  #Adding the difference in number of conflicts
        diffMSTotal = pccConflicts$conflictsTotal[, 1, drop = FALSE]
        for (j in 1:nrow(X$totalByMs)) {
            diffMSTotal[j, ] = pccConflicts$conflictsTotal[j, 1] - X$totalByMs[j, 1]
        }
        colnames(diffMSTotal) = "Difference"
        X$totalByMs = cbind(X$totalByMs, diffMSTotal)
    }
    X$conflictsDifferences = conflictsDifferences  #Summary of differences for the removal of each ms.
    colnames(X$conflictsDifferences) = "Conflicts differences"  #Remove the pause at plot option before finishing the function
    X$database = tableVariantes
    par(ask = FALSE)
    class(X) = "pccContam"
    return(X)  ##Pourquoi récupère-t-on une edgelist inutile?
}

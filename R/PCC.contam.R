PCC.contam <-
  function(x,
           omissionsAsReadings = FALSE,
           alternateReadings = FALSE,
           pauseAtPlot = FALSE) {#TODO: add a verbose option?
    ## Function for detecting contamination.
    ## For now, it computes the difference in total conflicts
    ## for the removal of each witness
    ## /!\ Very long execution time.
    ## For Ambroise, 290s*16 = 1h20
    ## Input: PCC.conflicts, overconflicting, or matrix
    ## 05/07/2014: added an option to pause at each plot
    X = as.list(NULL)
    if (is.matrix(x)) {
      # if x is a matrix, let's create a PCC.conflicts object
      x = PCC.conflicts(x,
                        omissionsAsReadings = omissionsAsReadings,
                        alternateReadings = alternateReadings)
    } else {
      if (class(x) == "pccEquipollent") {
        stop(
          "It does not make sense to apply PCC.contam()\n to an already equipollented database."
        )
      } else {
        {
          if (class(x) != "pccConflicts" &
              class(x) != "pccOverconflicting") {
            # TODO(JBC): There is a cleaner way to define methods for classes
            stop(
              "Input is neither a matrix, nor an object of class pccConflicts or pccOverconflicting."
            )
          }
        }
      }
    }
    tableVariantes = x$database
    X$totalByMs = x$conflictsTotal
    conflictsDifferences = as.matrix(integer(0))
    for (i in 1:ncol(tableVariantes)) {
      #Adding the new row to the synthesis of the conflicts differences
      database = tableVariantes[, -i, drop = FALSE]
      pccConflicts = PCC.conflicts(database,
                                   omissionsAsReadings = omissionsAsReadings,
                                   alternateReadings = alternateReadings)
      # Adding a label to the plot (if there is actually a plot, otherwise,
      # stating that there is no conflicts
      if (length(pccConflicts$edgelist) != 0) {
        graphics::title(main = "Conflicting variant locations",
                        sub = paste("Without ms.", colnames(tableVariantes)[i]))
      } else {
        #TODO: put this behind a verbose option?
        cat(
          "Without ms.",
          colnames(tableVariantes)[i],
          "there are NO CONFLICTS in the database.\n",
          "If the number of conflicts is otherwise high,\n",
          "it is likely that this manuscript is in cause\n"
        )
        if (pauseAtPlot == TRUE) {
          cat("Press [enter] to continue")
          line = readline()
        }
      }
      # Calculating the differences in conflict number
      difference = (sum(pccConflicts$conflictsTotal[, 1]) / 2) - 
        (sum(x$conflictsTotal[, 1] / 2) )
      conflictsDifferences = rbind(conflictsDifferences, difference)
      rownames(conflictsDifferences)[i] = colnames(tableVariantes)[i]  
      #Adding the conflicts per VL with this ms. removed
      colnames(pccConflicts$conflictsTotal)[1] = colnames(tableVariantes)[i]
      # Creating an object with the difference in centrality and total conflicts
      diffMSTotal = matrix(
        data = c(
          pccConflicts$conflictsTotal[, 1] - X$totalByMs[, 1],
          pccConflicts$conflictsTotal[, 2] - X$totalByMs[, 2]
          ), nrow = nrow(pccConflicts$conflictsTotal), ncol = 2,
        dimnames = list(
          rownames(pccConflicts$conflictsTotal),
          c(
            paste("d. confl.", colnames(tableVariantes)[i]), 
            paste("d. centr.", colnames(tableVariantes)[i])
            )
        ))
      X$totalByMs = cbind(X$totalByMs, diffMSTotal)
    }
    X$conflictsDifferences = conflictsDifferences  
    # Summary of differences for the removal of each ms.
    colnames(X$conflictsDifferences) = "Conflicts differences"
    X$database = tableVariantes
    class(X) = "pccContam"
    return(X)
  }

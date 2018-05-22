PCC.Exploratory <-
  function(x,
           omissionsAsReadings = FALSE,
           alternateReadings = FALSE,
           pauseAtPlot = FALSE,
           ask = TRUE,
           threshold = NULL,
           verbose = FALSE
           ) {
    # This is the global function for exploratory methods of the PCC type.
    # In entry, a matrix with a column per witness, and a row per variant location,
    # with readings coded with numbers.
    # TODO(JBC): une version prenant en compte ask = FALSE,
    # pour tout faire d'elle-même avec des valeurs moyennes (pas pour
    # tout de suite) - ou demander à l'utilisateur de les choisir plutôt
    # que de proposer des valeurs par défaut risquées
    if (!is.matrix(x)) {
      stop("Input must be a matrix.")
    }
    if (ask == FALSE & is.null(threshold)){
      stop("you must specify threshold with ask = FALSE")
    }
    tableVariantes = x
    pccConflicts = PCC.conflicts(tableVariantes,
                                 omissionsAsReadings = omissionsAsReadings,
                                 alternateReadings = alternateReadings)
    # If there are no conflicts, output directly
    if (sum(pccConflicts$conflictsTotal[, 1]) == 0) {
      return(pccConflicts)
    }
    # Simple interaction, which tests for value inputed by the user
    if(ask == TRUE){
      answered = FALSE
      while (answered == FALSE) {
        answerOne = readline("Do you want to proceed to the analysis of the network conflictuality ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
          print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
          return(pccConflicts)
        }
        if (answerOne == "Y") {
          answered = TRUE
        }
      }
    }
    if(ask){
      answered = FALSE
      while (answered == FALSE) {
        pccOverconflicting = PCC.overconflicting(pccConflicts, ask = ask, threshold = threshold)
        writeLines(
          "Are you satisfied with this configuration and do you want to\n Proceed to actual elimination of over-conflicting variant locations [P],\n  Try again with different value [T],\n or Quit [Q] ? \n "
        )
        reiterateQuestion = TRUE
        while (reiterateQuestion == TRUE) {
          answerOne = readline("(P/T/Q)")
          if (answerOne != "P" &&
              answerOne != "T" && answerOne != "Q") {
            print("Please enter P (Proceed), T (Try again) or Q (Quit).")
          }
          if (answerOne == "T") {
            reiterateQuestion = FALSE
          }
          if (answerOne == "Q") {
            return(pccOverconflicting)
          }
          if (answerOne == "P") {
            reiterateQuestion = FALSE
            answered = TRUE
          }
        }
      }
    } else{
      pccOverconflicting = PCC.overconflicting(pccConflicts, ask = ask, threshold = threshold)
    }
    pccElimination = PCC.elimination(pccOverconflicting)
    pccConflicts = PCC.conflicts(pccElimination,
                                 omissionsAsReadings = omissionsAsReadings,
                                 alternateReadings = alternateReadings)
    if (sum(pccConflicts$conflictsTotal[, 1]) == 0) {
      if(verbose){
        cat("There is no longer any conflict in the database. Function will stop.")
      }
      return(pccConflicts)
    }
    if(ask){
      cat(
        "There is still ",
        nrow(pccConflicts$edgelist),
        "conflicts in the database.\n",
        "If this number is high it might indicate that: \n",
        "1. The conflictuality index chosen was too low.\n", 
        "2. There are concurring structures in the tradition (due for instance to contamination)."
      )
      answered = FALSE
      while (answered == FALSE) {
        answerOne = readline("Do you want to proceed to contamination detection methods ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
          print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
          return(pccConflicts)
        }
        if (answerOne == "Y") {
          answered = TRUE
        }
      }
    }
    pccContam = PCC.contam(
      pccConflicts,
      pauseAtPlot = pauseAtPlot,
      omissionsAsReadings = omissionsAsReadings,
      alternateReadings = alternateReadings
    )
    if(ask){
      print(pccContam$conflictsDifferences)
      answered = FALSE
      while (answered == FALSE) {
        answerOne = readline("Do you want to define alternate configurations for stemma building ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
          print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
          return(pccContam)
        }
        if (answerOne == "Y") {
          answered = TRUE
          pccEquipollent = PCC.equipollent(pccConflicts, ask = ask, verbose = verbose)
          return(pccEquipollent)
        }
      }
    }
    # and now, in the ask = FALSE mode, we will try to decide smartly for the user
    # Either, there are one or several wits responsible for conficts, and 
    # we will equipollent for them, or, otherwise, we will try to do it
    # on a global basis
    myWits = rownames(pccContam$conflictsDifferences[ 
      pccContam$conflictsDifferences + (sum(pccContam$totalByMs[,1]) / 2)
      == 0, , drop = FALSE
      ])
    if(is.null(myWits)){
      pccEquipollent = PCC.equipollent(pccConflicts, ask = ask, scope = 'T',
                                       verbose = verbose)
    } else {
      pccEquipollent = PCC.equipollent(pccConflicts, ask = ask, scope = 'W',
                                       wits = myWits, verbose = verbose)
    }
    return(pccEquipollent)
  }

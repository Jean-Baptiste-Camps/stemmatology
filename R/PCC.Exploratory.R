PCC.Exploratory <-
function(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, pauseAtPlot = FALSE, 
    interactive = TRUE) {
    # TODO(JBC): dans ce groupe de fonction, comme dans le précédent, il est
    # impératif de tester que les options font bien leur job (y compris en
    # étant passées aux sous-fonctions) This is the global function for
    # exploratory methods of the PCC type.  In entry, a matrix with a column
    # per witness, and a row per variant location, with readings coded
    # with numbers.  TODO(JBC): une version prenant en compte interactive =
    # FALSE, pour tout faire d'elle-même avec des valeurs moyennes (pas pour
    # tout de suite) - ou demander à l'utilisateur de les choisir plutôt
    # que de proposer des valeurs par défaut risquées
    if (!is.matrix(x)) {
        stop("Input must be a matrix.")
    }
    tableVariantes = x
    pccConflicts = PCC.conflicts(tableVariantes, omissionsAsReadings = omissionsAsReadings, alternateReadings = alternateReadings)  # Simple interaction, which tests for value inputed by the user
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
    answered = FALSE
    while (answered == FALSE) {
        pccElimination = PCC.elimination(pccConflicts)
        writeLines("Are you satisfied with this configuration and do you want to\n Proceed to actual elimination of over-conflicting variant locations [P],\n  Try again with different value [T],\n or Quit [Q] ? \n ")
        reiterateQuestion = TRUE
        while (reiterateQuestion == TRUE) {
            answerOne = readline("(P/T/Q)")
            if (answerOne != "P" && answerOne != "T" && answerOne != "Q") {
                print("Please enter P (Proceed), T (Try again) or Q (Quit).")
            }
            if (answerOne == "T") {
                reiterateQuestion = FALSE
            }
            if (answerOne == "Q") {
                return(pccElimination)
            }
            if (answerOne == "P") {
                reiterateQuestion = FALSE
                answered = TRUE
            }
        }
    }
    pccDoElimination = PCC.doElimination(pccElimination)
    pccConflicts = PCC.conflicts(pccDoElimination, omissionsAsReadings = omissionsAsReadings, alternateReadings = alternateReadings)
    if (sum(pccConflicts$conflictsTotal[, 1]) == 0) {
        print("There is no longer any conflict in the database. Function will stop.")
        return(pccConflicts)
    }
    message = paste("There is still ", nrow(pccConflicts$edgelist), "conflicts in the database. If this number is high it might indicate that : \n 1. The conflictuality index chosen was too low. \n 2. There are concurring structures in the tradition (due for instance to contamination).")
    writeLines(message)
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
    pccContam = PCC.contam(pccConflicts, pauseAtPlot = pauseAtPlot, omissionsAsReadings = omissionsAsReadings, alternateReadings = alternateReadings)
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
        }
    }
    pccEquipollent = PCC.equipollent(pccConflicts)
    return(pccEquipollent)
}

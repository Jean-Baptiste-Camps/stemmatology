"PCC" <-
    function(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs = TRUE, pauseAtPlot = FALSE, interactive = TRUE) {
    # Global shell for the PCC functions. Successively calls PCC.Exploratory
    # and PCC.Stemma on the dataset NB: TODO(JBC) this function should be
    # updated when we will have defined appropriate object classes for the
    # various functions, and harmonised them.
    pccExploratory = PCC.Exploratory(x, omissionsAsReadings = omissionsAsReadings, 
        pauseAtPlot = pauseAtPlot, interactive = interactive)
    # Here, we will need to have appropriate version of the command for the
    # various outputs of PCC.Exploratory if is.pccDoElimination
    if (is.matrix(pccExploratory)) {
        output = pccStemma(pccExploratory)
    } else {
        if (!is.matrix(pccExploratory)) {
            # if is.pccConflicts|pccElimination|pccContam (with no alternate yet)
            if (class(pccExploratory) == "pccConflicts" 
              | class(pccExploratory) == "pccElimination" 
              | class(pccExploratory) == "pccContam") {
                output = PCC.Stemma(pccExploratory$database, limit = limit, recoverNAs = recoverNAs)
            } else {
                # if is.pccEquipollent
                if (class(pccExploratory) == "pccEquipollentDatabases") {
                  output = as.list(NULL)
                  for (i in 1:length(pccExploratory)) {
                    pccStemma = PCC.Stemma(pccExploratory[[i]])
                    legend("topright", paste("Alternative stemma", i, "out of", 
                      length(pccExploratory)))
                    if (i < length(pccExploratory)) {
                      readline("Press enter to proceed to next alternative stemma")
                    }
                    output[[i]] = pccStemma
                  }
                } else {
                  stop("Input is unknown.")
                }
            }
        }
    }
    return(output)
} 

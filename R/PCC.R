"PCC" <-
  function(x,
           omissionsAsReadings = FALSE,
           alternateReadings = FALSE,
           limit = 0,
           recoverNAs = TRUE,
           layout_as_stemma = FALSE,
           pauseAtPlot = FALSE,
           ask = TRUE,
           threshold = NULL,
           verbose = FALSE) {
    # Global shell for the PCC functions. Successively calls PCC.Exploratory
    # and PCC.Stemma on the dataset
    pccExploratory = PCC.Exploratory(
      x,
      omissionsAsReadings = omissionsAsReadings,
      alternateReadings = alternateReadings,
      pauseAtPlot = pauseAtPlot,
      ask = ask,
      threshold = threshold,
      verbose = verbose
    )
    # Here, we will need to have appropriate version of the command for the
    # various outputs of PCC.Exploratory if is.pccOverconflicting
    if (is.matrix(pccExploratory)) {
      #TODO: What does this do?
      output = pccStemma(pccExploratory)
    } else {
      if (!is.matrix(pccExploratory)) {
        # if is.pccConflicts|pccOverconflicting|pccContam (with no alternate yet)
        if (class(pccExploratory) == "pccConflicts"
            | class(pccExploratory) == "pccOverconflicting"
            | class(pccExploratory) == "pccContam") {
          output = PCC.Stemma(
            pccExploratory$database,
            limit = limit,
            recoverNAs = recoverNAs,
            layout_as_stemma = layout_as_stemma,
            ask = ask,
            verbose = verbose
          )
        } else {
          # if is.pccEquipollent
          if (class(pccExploratory) == "pccEquipollent") {
            if (is.null(pccExploratory$databases)) {
              stop(
                "It does not make sense to try to build a stemma on a presumably contaminated tradition if you did not define alternative configurations (using PCC.equipollent)"
              )
            }
            output = as.list(NULL)
            for (i in 1:length(pccExploratory$databases)) {
              pccStemma = PCC.Stemma(
                pccExploratory$databases[[i]],
                limit = limit,
                recoverNAs = recoverNAs,
                layout_as_stemma = layout_as_stemma,
                ask = ask,
                verbose = verbose
              )
              graphics::title(sub = paste(
                "Alternative stemma",
                i,
                "out of",
                length(pccExploratory$databases)
              ))
              if (pauseAtPlot && i < length(pccExploratory$databases)) {
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

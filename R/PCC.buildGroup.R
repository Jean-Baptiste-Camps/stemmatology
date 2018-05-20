PCC.buildGroup <-
function(x, limit = 0, ask = TRUE) {
    ### PCC.buildGroup Function that groups together manuscripts in relevant clusters
    ### where limit is the limit of severe disagreements (errores separativi) 
    ### allowed within a group (default - and strongly advised value - is 0). 
    ### Inputs are PCC.disagreement objects.
    severeDisagreement = x$severeDisagreement  #We remove NA from the severeDisagreement list,
    # thus starting to distinguish between NA and 0.
    # severeDisagreement[is.na(severeDisagreement)] = 0;
    if (length(severeDisagreement[is.na(severeDisagreement)]) > 0) {
        if (ask == TRUE){
          print("There are NA values in your severeDisagreement matrix.")
          answered = FALSE
          writeLines("Do you whish to proceed anyway (careful !) ?")
          while (answered == FALSE) {
            answer = readline("(Y/N)")
            if (answer != "N" && answer != "Y") {
              print("Please enter Y (yes) or N (no).")
            }
            if (answer == "N") {
              stop("No groups will be created")
            }
            if (answer == "Y") {
              answered = TRUE
            }
          }
        }
      #If the session is not interactive or the user agree, we remove NA's
      #i.e., we assume that no information about disagreement means "no disagreement".
      severeDisagreement[is.na(severeDisagreement)] = 0
      #TODO: would it be better to keep NA and continue this way, treating
      #them differently (e.g., not using them for grouping?) -> I think not.
    }
    groups = as.list(NULL)
    for (i in 1:nrow(severeDisagreement)) {
        groups[[i]] = labels(severeDisagreement[i,])[severeDisagreement[i, 
            ] <= limit]
    }
    # Removing entries with single ms
    groups = groups[lengths(groups) > 1]
    # Removing duplicates
    groups = unique(groups)
    if(!is.null(groups)){
      # And here, we add a final test to check if there are unvalid
      # configurations in which some members are in conflict between
      # themselves, in which case we will remove them.
      # TODO: improve this !
      toBeRemovedAsWell = as.vector(NULL)
      for (l in seq_len(length(groups))) {
        # We look in the severe desagreement table for all witnesses
        # in the group. If any are superior to limit, we have an
        # unexpected configuration
        if (any(severeDisagreement[groups[[l]],groups[[l]]] > limit)){
          # TODO: give more explanation? i.e., some witnesses
          # inside the group have severe disagreement(s) between
          # themselves, which should not happen.
          # This usually means insufficient data, or 
          # weird/polygenetic/contaminated variant locations
          warning("Unexpected configuration in the group: ", 
                  groups[[l]], 
                  ".\nThis group will be ignored.",
                  "\nThis can be caused by insufficient data or anomalous variant locations.",
                  "\nN.B.: To circumvent this, add data, eliminate problematic readings,\n or, if necessary, modify the value of 'limit'."
          )
          toBeRemovedAsWell = c(toBeRemovedAsWell, l)
          #TODO: give more information? e.g., the witnesses that are in disagr. ?
        }
      }
      # And finally, we remove them.
      if (!is.null(toBeRemovedAsWell)) {
        groups = groups[-toBeRemovedAsWell]
      }
    }
    output = as.list(NULL)
    output$database = x$database
    output$groups = groups
    return(output)
}

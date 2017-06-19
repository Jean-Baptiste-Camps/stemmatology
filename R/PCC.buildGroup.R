PCC.buildGroup <-
function(x, limit = 0) {
    ### PCC.buildGroup Function that groups together manuscripts in relevant clusters, then
    ### identify and eventually reconstructs their model # where limit is the
    ### limit of severe disagreements (errores separativi) allowed within a
    ### group (default - and strongly advised value - is 0). Inputs are PCC.disagreement objects.
    severeDisagreement = x$severeDisagreement  #We remove NA from the severeDisagreement list,
    # thus starting to distinguish between NA and 0.
    # severeDisagreement[is.na(severeDisagreement)] = 0;
    if (length(severeDisagreement[is.na(severeDisagreement)]) > 0) {
        print("There are NA values in your severeDisagreement matrix.")
        answered = FALSE
        writeLines("Do you whish to proceed anyway (careful !) ?")
        while (answered == FALSE) {
            answer = readline("(Y/N)")
            if (answer != "N" && answer != "Y") {
                print("Please enter Y (yes) or N (no).")
            }
            if (answer == "N") {
                return()
            }
            if (answer == "Y") {
              #if answer is yes, we assume that no information about disagreement means "no disagreement".
                severeDisagreement[is.na(severeDisagreement)] = 0
                answered = TRUE
            }
        }
       
    }
    groups = as.list(NULL)
    for (i in 1:nrow(severeDisagreement)) {
        groups[[i]] = labels(severeDisagreement[i, severeDisagreement[i, 
            ] == limit])
    }
    # Comparing all lists and deleting identical lines.
    toBeRemoved = as.vector(NULL)
    for (j in 1:(length(groups) - 1)) {
        for (k in (j + 1):length(groups)) {
            if (identical(groups[[j]], groups[[k]])) {
                toBeRemoved = c(toBeRemoved, k)
            }
        }
    }
    if (!is.null(toBeRemoved)) {
        groups = groups[-toBeRemoved]
    }
    # And here, we add a final test to check if there are unvalid
    # configurations in which some members are in conflict between
    # themselves, in which case we will remove them.
    toBeRemovedAsWell = as.vector(NULL)  #If there is more than two members
    for (l in 1:length(groups)) {
        # Added this test to avoid weird '1' popping when there are actually no
        # other 0 in the line other than the tested ms. with himself
        if ((length(groups[[l]]) == 1) && (groups[[l]] == 1)) {
            toBeRemovedAsWell = c(toBeRemovedAsWell, l)
        }
        # If there are more than two members
        if (length(groups[[l]]) > 2) {
            # We test for problematic configurations
            problems = 0
            for (m in 1:(length(groups[[l]]) - 1)) {
                for (n in (m + 1):length(groups[[l]])) {
                  # We confront them in the severeDisagreement table
                  if (severeDisagreement[groups[[l]][m], groups[[l]][n]] > 
                    0) {
                    problems = problems + 1
                  }
                }
            }
            if (problems > 0) {
                message = paste("there is an unexpected configuration ; we will remove this group from the list altogether. It concerns mss:")
                print(message)
                print(groups[[l]])
                toBeRemovedAsWell = c(toBeRemovedAsWell, l)
            }
        }
    }
    # And finally, we remove them.
    if (!is.null(toBeRemovedAsWell)) {
        groups = groups[-toBeRemovedAsWell]
    }
    output = as.list(NULL)
    output$database = x$database
    output$groups = groups
    return(output)
}

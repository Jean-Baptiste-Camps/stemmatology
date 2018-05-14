PCC.elimination <-
function(x) {
    ### Takes in input an object of class pccOverconflicting, and
    ### deletes variant locations labelled as overconflicting;
    ### outputs the new database.
    # we select the overconflicting
    overconflicting = rownames(x$vertexAttributes)[x$vertexAttributes[, 1] == "overconflicting"]
    # and we remove them
    x$database = x$database[!rownames(x$database) %in% overconflicting, , 
        drop = FALSE]
    return(x$database)
}

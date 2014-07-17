PCC.doElimination <-
function(x) {
    ### Fonction qui prend en entrée un objet de type PCC.elimination, et
    ### procède à la suppression des lieux variants étiquetés comme
    ### overconflicting, et donne en sortie la nouvelle base de données
    ### (seulement ; il faut donc rappeler ensuite PCC.conflicts, etc.) On crée
    ### une liste des étiquettes des lieux variants overconflicting
    overconflicting = NULL  #on teste
    for (i in 1:nrow(x$vertexAttributes)) {
        if (x$vertexAttributes[i, 1] == "overconflicting") {
            overconflicting = c(overconflicting, rownames(x$vertexAttributes)[i])
        }
    }
    # and we remove them
    x$database = x$database[!rownames(x$database) %in% overconflicting, , 
        drop = FALSE]
    return(x$database)
}

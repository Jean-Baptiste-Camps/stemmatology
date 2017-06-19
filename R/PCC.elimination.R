PCC.elimination <-
function(x) {
    ### Fonction qui prend en entrée un objet de type pccOverconflicting, et
    ### procède à la suppression des lieux variants étiquetés comme
    ### overconflicting, et donne en sortie la nouvelle base de données
    ### (seulement ; il faut donc rappeler ensuite PCC.conflicts, etc.)
    ### Cette fonction, de dimensions très restreintes, pourrait peut-être
    ### aller entièrement dans PCC.elimination (en option) ?
    ### => à la réflexion, non, car il peut y avoir des usages où l'on 
    ### souhaite juste tracer le réseau coloré des conflits, ou des usages
    ### où l'on veut itérer l'estimation des overconflicting sans éliminer
    # we select the overconflicting
    overconflicting = rownames(x$vertexAttributes)[x$vertexAttributes[, 1] == "overconflicting"]
    # and we remove them
    x$database = x$database[!rownames(x$database) %in% overconflicting, , 
        drop = FALSE]
    return(x$database)
}

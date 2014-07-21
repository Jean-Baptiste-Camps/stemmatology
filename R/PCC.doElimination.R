PCC.doElimination <-
function(x) {
    ### Fonction qui prend en entrée un objet de type PCC.elimination, et
    ### procède à la suppression des lieux variants étiquetés comme
    ### overconflicting, et donne en sortie la nouvelle base de données
    ### (seulement ; il faut donc rappeler ensuite PCC.conflicts, etc.)
    ### Cette fonction, de dimensions très restreintes, pourrait peut-être
    ### aller entièrement dans PCC.elimination... (permet de diminuer le 
    ### nombre de fonctions)
    ### Cette fonction est relativement rapide, mais on peut peut-être l'optimiser en vectorisant la boucle
    # gains (microbenchmark, 100 iter)
#     Unit: microseconds
#     expr    min       lq   median       uq     max    neval
#     V1     410.19 415.0665 422.7815 441.4395 852.865   100
#     V2      81.31  83.0445  83.9625  85.8750 180.253   100
    overconflicting = rownames(x$vertexAttributes)[x$vertexAttributes[, 1] == "overconflicting"]
#     overconflicting = NULL  #on teste
#     for (i in 1:nrow(x$vertexAttributes)) {
#         if (x$vertexAttributes[i, 1] == "overconflicting") {
#             overconflicting = c(overconflicting, rownames(x$vertexAttributes)[i])
#         }
#     }
    # and we remove them
    x$database = x$database[!rownames(x$database) %in% overconflicting, , 
        drop = FALSE]
    return(x$database)
}

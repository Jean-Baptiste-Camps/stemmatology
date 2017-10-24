VL.pValues <-
function(x) {
    tableVariantes = as.matrix(x, dimnames = labels(x))
    tableVariantes[is.na(tableVariantes)] = 0
    pValueTable = matrix(nrow = nrow(tableVariantes), ncol = nrow(tableVariantes), 
        dimnames = c(labels(tableVariantes)[1], labels(tableVariantes)[1]))
    totalpValue = matrix(nrow = nrow(tableVariantes), ncol = 1, dimnames = c(labels(tableVariantes)[1], 
        "Total inferor to 0,05"))
    for (i in 1:nrow(tableVariantes)) {
        testTable = tableVariantes[-i, , drop = FALSE]
        totalpValue[i, ] = 0
        for (j in 1:nrow(testTable)) {
            fisherResult = fisher.test(tableVariantes[i, ], testTable[j, 
                ])
            pValue = fisherResult$p.value
            pValueTable[rownames(tableVariantes)[i], rownames(testTable)[j]] = pValue
            if (pValue < 0.051) {
                totalpValue[i, ] = totalpValue[i, ] + 1
            }
        }
    }
    pValueTable = cbind(pValueTable, totalpValue)  ###Début du code plot # C'est quoi déjà ce graphique #Ah oui, représentation visuelle de l'évolution de la pertinence des variantes
    courbeSignificativite = apply(pValueTable, MARGIN = 1, mean, na.rm = TRUE)
    plot(courbeSignificativite, type = "l")
    ### fin du code plot
    return(pValueTable)
}

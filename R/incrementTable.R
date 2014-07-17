incrementTable <-
function(tableToIncrement, tableVariantes, x, y) {
    ### An «internal» function used to increment tables according to manuscript
    ### sigla Soit la valeur est NA et on la crée, soit elle existe déjà et on
    ### l'incrémente
    if (is.na(tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]])) {
        tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]] = 1
    } else {
        tableToIncrement[colnames(tableVariantes)[x], colnames(tableVariantes)[y]] = tableToIncrement[colnames(tableVariantes)[x], 
            colnames(tableVariantes)[y]] + 1
    }
    return(tableToIncrement)
}

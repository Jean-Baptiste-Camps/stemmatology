Import.TEIApparatus <-
function(file = x, fromCollatex = False) {
    ### Ajout d'une fonction d'import de document TEI encodé conformément à nos
    ### préconisations.  NB: si on veut vraiment garder la fonction import.TEI
    ### (souhaitable à terme), il faudra vraiment - convertir en XSLT 1.0 la
    ### feuille écrite en XSLT 2.0 - créer un fichier CONFIGURE pour vérifier
    ### la présence de libxslt (Unix) et des merdouilles équivalentes sur
    ### Windows Il existe une librairie Sxslt Le problème est néanmoins qu'elle
    ### n'accepte pas XSLT 2.0 dont nous avons pour l'instant besoin...  À
    ### terme, pourrait être intéressant d'ajouter une option «from CollateX»
    ### pour l'import teiDoc = = readLines(x) #N'est plus utile si on utilise
    ### la commande xsltproc... Maintenant, il faut trouver un moyen d'appeler
    ### xslt Version qui devrait fonctionner avec Linux et avec XSLT 1.0...
    commandeXSLT = paste("xsltproc genere-Tableau-JBCamps-avec-exclusion-des-types_v3_cours.xsl ", 
        x)
    parsedTEI = system(commandeXSLT, intern = TRUE)
    tableVariantes = read.csv(textConnection(parsedTEI), header = TRUE, row.names = 1, 
        sep = ";")
    return(tableVariantes)
}

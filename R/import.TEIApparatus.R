import.TEIApparatus <-
  function(file = "",
           appTypes = c("substantive")) {
    #TODO: add the possibility to import from a collatex output without listWit?
    # load xml document
    doc = xml2::read_xml(file)
    # for the moment, I strip the default namespaces, to deal with poorly made XML
    xml2::xml_ns_strip(doc)
    # Find all witnesses
    myWits = xml2::xml_text(xml2::xml_find_all(doc,
                                   "/descendant::listWit/descendant::witness/@xml:id"))
    # alphabetic sort
    myWits = sort(myWits)
    # create the matrix
    tableVariantes = matrix(
      nrow = 0,
      ncol = length(myWits),
      dimnames = list(NULL, myWits)
    )
    # and now, let's get the app values
    if (is.null(appTypes)) {
      myApps = xml2::xml_find_all(doc, "/descendant::app")
    } else {
      myTypes = paste("@type = '",
                      appTypes,
                      "'",
                      sep = "",
                      collapse = " or ")
      myXpath = paste("/descendant::app[", myTypes, "]", sep = "")
      myApps = xml2::xml_find_all(doc, xpath = myXpath)
    }
    # and transform them
    for (i in seq_len(length(myApps))) {
      # create a list of rdg with their witnesses,
      # and a second with their codes
      myRdgs = xml2::xml_find_all(myApps[i], "./rdg")
      myRdgWits = xml2::xml_text(xml2::xml_find_all(myApps[i], "./rdg/@wit"))
      myRdgWits = gsub("#", " ", myRdgWits)
      myRdgWits = sub("^\\s+|\\s+$", "", myRdgWits)
      myRdgWits = strsplit(myRdgWits, "\\s+")
      myRdgVars = NULL
      # 0 for omission, 1...n for readings
      for (j in seq_len(length(myRdgs))) {
        if (xml2::xml_text(myRdgs[j], "normalize-space(.)") == "") {
          myRdgVars = c(myRdgVars, 0)
        }
        else{
          myRdgVars = c(myRdgVars, j)
        }
      }
      myRow = NULL
      for (j in seq_len(length(myWits))) {
        # Check if the wit siglum is contained
        # in one or some of the lists,
        # and, if it is the case, attribute its reading codes
        # NB: could be made more readable, but perhaps less performant
        # with loops
        myWitReading = myRdgVars[which(sapply(
          myRdgWits,
          FUN = function(X)
            myWits[j] %in% X
        ))]
        # and now, for NA and multiple readings
        if (length(myWitReading) == 0) {
          myWitReading = NA
        }
        if (length(myWitReading) > 1) {
          myWitReading = paste(as.character(myWitReading), collapse = ",")
        }
        myRow = c(myRow, myWitReading)
      }
      # and now, give my row a label
      # if it has an xml:id, we take that, otherwise
      # we take the index of the row
      myLabel = xml2::xml_text(xml2::xml_find_all(myApps[i], "@xml:id"))
      if (length(myLabel) == 0) {
        myLabel = as.character(i)
      }
      tableVariantes = rbind(tableVariantes, myRow)
      rownames(tableVariantes)[i] = myLabel
    }
    return(tableVariantes)
  }

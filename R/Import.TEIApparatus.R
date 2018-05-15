Import.TEIApparatus <-
  function(file = "",
           appTypes = c("substantive")) {
    
    # load xml document
    doc = read_xml(file)
    # for the moment, I strip the default namespaces, to deal with poorly made XML
    xml_ns_strip(doc)
    # Find all witnesses
    myWits = xml_text(xml_find_all(doc, 
                      "/descendant::listWit/descendant::witness/@xml:id"))
    # alphabetic sort
    myWits = sort(myWits)
    
    # create the matrix
    tableVariantes = matrix(nrow = 0, ncol = length(myWits), 
                            dimnames = list(NULL, myWits))
    
    # and now, let's get the app values
    myApps = xml_find_all(doc, "/descendant::app")
    # and transform them
    for(i in seq_len(length(myApps))){
      
      # create a list of rdg with their position, their witness,
      # and their type
      myRdgs = xml_find_all(myApps[i], "./rdg")
      myRdgWits = xml_text(xml_find_all(myApps[i], "./rdg/@wit"))
      myRdgWits = gsub("#", " ",myRdgWits)
      myRdgWits = sub("^\\s+|\\s+$", "",myRdgWits)
      myRdgWits = strsplit(myRdgWits, "\\s+")
      
      myRow = NULL
      for(j in seq_len(length(myWits))){
        # do the wit appear?
        
        # is it an omission?
        
        # does it appear several time?
      }
    }
    
   
    return(tableVariantes)
  }

expandVL <- function(x){
  # Transform a matrix with alternateReadings in a (bigger)
  # matrix without them, by expanding every VL with alternateReadings
  # into a series of simple VL
  
  if(!is.matrix(x) && !is.character(x)){
    stop("Input must be a character matrix with alternate readings.")
  }
  
  output = matrix(nrow = 0, ncol = ncol(x), dimnames = list(NULL, colnames(x)))
  
  for(i in seq_len(nrow(x))){
    myVL = x[i,, drop = FALSE]
    if(any(grepl(pattern = ",", myVL, fixed = TRUE))){
      while(any(grepl(pattern = ",", myVL, fixed = TRUE))){
        # placeholder for new rows
        # TODO: remove dimnames once code completed
        newRows = matrix(nrow = 0, ncol = ncol(x), dimnames = list(NULL, colnames(x)))
        # First, locate the first value with alternateReadings
        k = which(grepl(pattern = ",", myVL[1,], fixed = TRUE))[1]
        # get the atomic values
        myValues = unlist(strsplit(myVL[1,k], split = ",", fixed = TRUE))
        # for every row of our VL being expanded
        # (i.e., only one at first pass, more after)
        # deduplicate for this value
        for(j in seq_len(nrow(myVL))){
          # And now that we have the multiple values for this row, 
          # we create as many new VLs as necessary
          for(l in seq_len(length(myValues))){
            # copy the row
            myNewRow = myVL[j,,drop = FALSE]
            # replace with single value
            myNewRow[k] = myValues[l]
            # add it
            newRows = rbind(newRows,myNewRow)
          }
        }
        myVL = newRows
      }
      mode(myVL) = 'numeric'
      output = rbind(output, myVL)
    } else {
      mode(myVL) = 'numeric'
      output = rbind(output, myVL)
    }
  }
  
  return(output)
  
}

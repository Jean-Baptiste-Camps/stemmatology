context("PCC.conflicts")

test_that("PCC.conflicts works properly on basic setting", {
  
  x = matrix(
    data = c(
      1,1,1,1,
      1,2,0,2,
      2,1,1,1,
      2,2,0,2,
      2,3,0,NA
    ),
    ncol = 5,
    nrow = 4,
    dimnames = list(
      c("VL1","VL2","VL3","VL4"),
      c("A","B","C","D","E")
    )
  )
  
  results = list(edgelist = 
    structure(c("VL1", "VL1", "VL2", "VL4"), 
              .Dim = c(2L, 2L)), 
    conflictsTotal = structure(c(2, 1, 0, 1, 2, 1, 0, 1), 
      .Dim = c(4L, 2L), 
      .Dimnames = list(c("VL1", "VL2","VL3", "VL4"), c("Number of conflicts", "Centrality index"))), 
    database = x)
  
  class(results) = "pccConflicts"
  
  expect_equal(PCC.conflicts(x), results)
  
  # And now with omissionsAsReadings
  
  results = list(edgelist = 
    structure(c("VL1", "VL1", "VL1", "VL2", "VL3", "VL4"), 
      .Dim = c(3L, 2L)), 
    conflictsTotal = structure(c(3, 1, 1, 1, 2, 0.5, 0.5, 0.5), 
      .Dim = c(4L, 2L), 
      .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), c("Number of conflicts", "Centrality index"))),
    database = x)
  
  class(results) = "pccConflicts"
  
  expect_equal(PCC.conflicts(x, omissionsAsReadings = TRUE), results)
  
})

# variants
#     A B C D E
# VL1 1 1 2 2 2
# VL2 1 2 1 2 3
# VL3 1 0 1 0 0
# VL4 1 2 1 2 NA 

test_that("PCC.conflicts works with alternateReadings",{
  # very simple case with internal contradiction
  # variants
  #       A    B   C   D   E
  # VL1 "1,2" "1" "2" "2" "2"
  x = matrix(data=c("1,2","1","2","2","2"), ncol = 5, nrow = 1, 
    dimnames = list(c("VL1"),c("A","B","C","D","E")))
  
  # TODO: test à ajouter quand on se sera décidés sur la mesure de
  # centralité
  #PCC.conflicts(x, alternateReadings = TRUE)
  #bugue, et 2e bug si on fait, rbind(x,x)
  
  #PCC.conflicts(rbind(x,x), alternateReadings = TRUE)
  
  # More detailed case
  x = matrix(
    data = c(
      "1,2","1","1","1","1,2","1",
      "1","2","0","2",NA,"2",
      "2","1","1","1",NA,"2",
      "2","2","0","2",NA,"2",
      "2","3","0",NA,NA,"3"
    ),
    ncol = 5,
    nrow = 6,
    dimnames = list(
      c("VL1","VL2","VL3","VL4","VL5","VL6"),
      c("A","B","C","D","E")
    )
  )
  
  results = list(
    edgelist = structure(
      c("VL1", "VL1", "VL1", "VL1", "VL1","VL5",
        "VL1", "VL2", "VL4", "VL5", "VL6","VL5"),
      .Dim = c(6L, 2L)), 
    conflictsTotal = 
      structure(c(6, 1, 0, 1, 3, 1, 
                  2,0.2, 0, 0.2, 1, 0.2), 
      .Dim = c(6L, 2L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6"), c("Number of conflicts", "Centrality index"))), 
    database = x)
  class(results) = "pccConflicts"
    
  expect_equal(PCC.conflicts(x, alternateReadings = TRUE), results)
  
})

#       A    B   C   D   E
# VL1 "1,2" "1" "2" "2" "2"
# VL2  "1"  "2" "1" "2" "3"
# VL3  "1"  "0" "1" "0" "0"
# VL4  "1"  "2" "1" "2"  NA
# VL5 "1,2" NA  NA  NA   NA
# VL6  "1"  "2" "2" "2"  "3"

#TODO: test
# - alternateReadings
# - conjunction alternateReadings / omissionsAsReadings
# - expect a plot
# - test if input is checked correctly


#TODO: check fournival output
test_that("PCC.conflicts correct output on Fournival", {
  data("fournival")
  myConflicts = PCC.conflicts(fournival)
  expect_equal_to_reference(myConflicts, file="myConflicts.rds")
})



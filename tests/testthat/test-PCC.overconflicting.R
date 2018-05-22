context("PCC.overconflicting")

test_that("PCC.overconflicting yields correct output on simple input", {
  x = list(
    edgelist = structure(c("VL1", "VL1", "VL2", "VL4"), 
      .Dim = c(2L, 2L)), 
    conflictsTotal = structure(c(2, 1, 0, 1, 2, 1, 0, 1), 
      .Dim = c(4L, 2L), 
      .Dimnames = list(c("VL1", "VL2","VL3", "VL4"), c("Number of conflicts", "Centrality index"))), 
    database = matrix(
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
    ))
  class(x) = "pccConflicts"
  
  results = list(
    edgelist = structure(c("VL1", "VL1", "VL2", "VL4"), 
                         .Dim = c(2L, 2L)), 
    conflictsTotal = structure(c(2, 1, 0, 1, 2, 1, 0, 1), 
                               .Dim = c(4L, 2L), 
                               .Dimnames = list(c("VL1", "VL2","VL3", "VL4"), c("Number of conflicts", "Centrality index"))), 
    database = matrix(
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
    ),
    vertexAttributes = structure(
      c("overconflicting", "sober", "sober", "red", "green", "green"), 
      .Dim = c(3L, 2L), .Dimnames = list(c("VL1", "VL2","VL4"), c("label", "color")))
    )
  class(results) = "pccOverconflicting"
  
  # expect correct results and a message
  expect_equal(
    expect_message(PCC.overconflicting(x, ask = FALSE, threshold = 1)), 
    results)
  # and now see if the checks for correct input work properly
  expect_error(PCC.overconflicting(x, ask = FALSE))
  expect_error(PCC.overconflicting(x, ask = FALSE, threshold = '1'))
  # what follows to be put to use when we make threshold computation evolve
  #expect_error(PCC.overconflicting(x, ask = FALSE, threshold = 100))
  #expect_error(PCC.overconflicting(x, ask = FALSE, threshold = -1))
})

test_that("yields correct output on harder case with self conflicts", {
  x = list(
      edgelist = structure(
        c("VL1", "VL1", "VL1", "VL1", "VL1","VL5",
          "VL1", "VL2", "VL4", "VL5", "VL6","VL5"),
        .Dim = c(6L, 2L)), 
      conflictsTotal = 
        structure(c(6, 1, 0, 1, 3, 1, 
                    2,0.2, 0, 0.2, 1, 0.2), 
                  .Dim = c(6L, 2L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6"), c("Number of conflicts", "Centrality index"))), 
      database = NULL #we won't check that for the moment
      )
  class(x) = "pccConflicts"
  
  vertexAttributes = structure(
    c("overconflicting", "sober", "sober", "overconflicting", "sober", 
      "red", "green", "green", "red", "green"), 
    .Dim = c(5L, 2L), 
    .Dimnames = list(c("VL1", "VL2", "VL4", "VL5", "VL6"), c("label","color")))
  
  expect_equal(PCC.overconflicting(x, ask = FALSE, threshold = 0.6)$vertexAttributes, vertexAttributes)

})

test_that("inputs are checked correctly", {
  # Checking input class
  x = list(
    edgelist = structure(c("VL1", "VL1", "VL2", "VL4"), 
                         .Dim = c(2L, 2L)), 
    conflictsTotal = structure(c(2, 1, 0, 1, 2, 1, 0, 1), 
                               .Dim = c(4L, 2L), 
                               .Dimnames = list(c("VL1", "VL2","VL3", "VL4"), c("Number of conflicts", "Centrality index"))), 
    database = matrix(
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
    ))
  #class(x) = "pccConflicts"
  expect_error(PCC.overconflicting(x))
  
  # Test for input with no conflict?
  
})

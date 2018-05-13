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

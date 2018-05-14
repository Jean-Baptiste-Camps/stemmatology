context("PCC.elimination")

test_that("yields correct output on simple object", {
  x = list(
    edgelist = structure(c("VL1", "VL1", "VL2", "VL4"), 
      .Dim = c(2L, 2L)), 
    conflictsTotal = structure(c(2, 1, 0, 1, 2, 1, 0, 1), 
      .Dim = c(4L, 2L), 
      .Dimnames = list(c("VL1", "VL2","VL3", "VL4"), 
                       c("Number of conflicts", "Centrality index"))), 
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
  class(x) = "pccOverconflicting"
  
  results = matrix(
    data = c(
      1,1,1,
      2,0,2,
      1,1,1,
      2,0,2,
      3,0,NA
    ),
    ncol = 5,
    nrow = 3,
    dimnames = list(
      c("VL2","VL3","VL4"),
      c("A","B","C","D","E")
    ))
  
  expect_equal(PCC.elimination(x), results)
  
})

# TODO: more tests?

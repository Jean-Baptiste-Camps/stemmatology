context("PCC.Exploratory")

test_that("yields expected output on fournival", {
  # Skipping the following test on CRAN, as it will take too long
  skip_on_cran()
  data("fournival")
  expect_equal_to_reference(expect_output(
    PCC.Exploratory(
      fournival,
      ask = FALSE,
      threshold = 0.06,
      verbose = FALSE
    )
  ),
  file = "equipollent_fourni_witD.rds")
})

test_that("does not fail when there are no conflicts at all in the tradition", {
  x = matrix(
    c(
      1,0,1,1,1,1,1,1,
      1,0,1,2,2,2,1,2,
      1,0,0,3,2,1,NA,3,
      2,0,1,4,NA,1,1,1,
      2,1,2,5,2,1,1,4
    ), nrow = 8, ncol = 5,
    dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7","VL8"),
                    c("A","B","C","D","E")))
  results = list(
    edgelist = structure(character(0), .Dim = c(0L,2L)), 
    conflictsTotal = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
          .Dim = c(8L, 2L), 
          .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), 
                           c("Number of conflicts", "Centrality index"))),
    database = x)
  class(results) = "pccConflicts"
  expect_equal(expect_message(PCC.Exploratory(x)), results)
  
})

test_that("user input is checked correctly", {
  
  x = data.frame(x = 1, y = 1:10)
  expect_error(PCC.Exploratory(x))
  
  x = matrix(1:10, nrow = 2)
  expect_error(PCC.Exploratory(x, ask = FALSE))
  
  #TODO: check for colnames / rownames
})

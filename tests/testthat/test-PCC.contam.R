context("PCC.contam")

test_that("works on numeric matrix input", {
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
  
  results = list(
    totalByMs = 
      structure(c(2, 1, 0, 1, 2, 1, 0, 1, -2, -1, 0, -1, -2, -1, 0, -1, -2, -1, 0, -1, -2, -1, 0, -1, -2, -1, 0, -1, -2, -1, 0, -1,-2, -1, 0, -1, -2, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 
        .Dim = c(4L,12L), 
        .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), 
          c("Number of conflicts", "Centrality index", "d. confl. A", "d. centr. A", "d. confl. B", "d. centr. B", "d. confl. C", "d. centr. C", "d. confl. D", "d. centr. D", "d. confl. E", "d. centr. E")
          )),
  conflictsDifferences = structure(c(-2, -2, -2, -2, 0), 
    .Dim = c(5L, 1L), .Dimnames = list(
    c("A", "B", "C", "D", "E"), "Conflicts differences")),
  database = x)
  class(results) = "pccContam"
  
  # expect output as well, or create option verbose
  expect_equal(
    expect_output(
      expect_message(
        PCC.contam(x)
        )
      )
    , results)
})


test_that("works on character matrix input", {
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
  
  # Check if input properly verified
  expect_error(PCC.contam(x))
  
  # And now check correct result
  PCC.contam(x, alternateReadings = TRUE)
  
})

test_that("works on pccConflicts input", {
  
})

test_that("works on pccOverConflicting input", {
  
})

test_that("works on larger input", {
  
})

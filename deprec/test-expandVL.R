context("expandVL")

test_that("works for a single row", {
  x = structure(c("1,2", "1,2,3", "2,3", "2", "0,2"), 
    .Dim = c(1L, 5L), .Dimnames = list("VL1", c("A", "B", "C", "D", "E")))
  
  results = 
    matrix(byrow = TRUE,
      data = c(
      1, 1, 2, 2, 0,
      1, 1, 2, 2, 2,
      1, 1, 3, 2, 0,
      1, 1, 3, 2, 2,
      1, 2, 2, 2, 0,
      1, 2, 2, 2, 2,
      1, 2, 3, 2, 0,
      1, 2, 3, 2, 2,
      1, 3, 2, 2, 0,
      1, 3, 2, 2, 2,
      1, 3, 3, 2, 0,
      1, 3, 3, 2, 2,
      2, 1, 2, 2, 0,
      2, 1, 2, 2, 2,
      2, 1, 3, 2, 0,
      2, 1, 3, 2, 2,
      2, 2, 2, 2, 0,
      2, 2, 2, 2, 2,
      2, 2, 3, 2, 0,
      2, 2, 3, 2, 2,
      2, 3, 2, 2, 0,
      2, 3, 2, 2, 2,
      2, 3, 3, 2, 0,
      2, 3, 3, 2, 2
    ),  ncol = 5, 
    dimnames = list(
      rep("VL1", 24),
      c("A","B","C","D","E")
    )
    )
  
  expect_equal(expandVL(x), results)
  
})

test_that("works for a larger matrix", {
  
  x = matrix(
    data = c(
      "1,2","1","1","1","1,2","1",
      "1,2,3","2","0","2",NA,"2",
      "2,3","1","1","1",NA,"2",
      "2","2","0","2",NA,"2",
      "0,2","3","0",NA,"1,2","3"
    ),
    ncol = 5,
    nrow = 6,
    dimnames = list(
      c("VL1","VL2","VL3","VL4","VL5","VL6"),
      c("A","B","C","D","E")
    )
  )
  
  expandVL(x)

})

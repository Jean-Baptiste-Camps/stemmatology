context("layout_as_stemma")

test_that("Layout as stemma works", {
  
  edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", "A", "B", "C", "E", 1,5,3,10), .Dim = c(4L, 3L)
  )
  
  expect_equal(
    object = layout_as_stemma(edgelist), 
    expected = structure(
      c(-1, -2, -1, 0, 1, 1, 
        1, 0, -4, -2, 1, -9), 
      .Dim = c(6L, 2L))
      )
  
})

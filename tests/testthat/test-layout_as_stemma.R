context("layout_as_stemma")

test_that("Layout as stemma works", {
  
  edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", 
        "A", "B", "C", "E", 
         1,5,3,10), .Dim = c(4L, 3L)
  )
  
  expect_equal(
    object = layout_as_stemma(edgelist), 
    expected = structure(
      c(-1, -2, -1, 0, 1, 1, 
        1, 0, -4, -2, 1, -9), 
      .Dim = c(6L, 2L))
      )
  
  # With more levels
  edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", "A","A","G",
        "A", "B", "C", "E", "F","G","H",
        1,5,3,10,3,4,5), .Dim = c(7L, 3L)
  )
  
  layout = layout_as_stemma(edgelist)
  
  expect_equal(layout, structure(c(-1, -2, -1, 0, 1, 1, -2.5, -1.5, -1.5, 
                                   3, 2, -2, 0, 3, -7, -1, -2, -7), .Dim = c(9L, 2L)))
  
  # With simple to handle contamination
   edgelist = structure(
     c("{ABC}", "{ABC}", "{ABC}", "D", "A","A","F",
       "A", "B", "C", "E", "F","G","G",
       1,5,3,10,3,4,2), .Dim = c(7L, 3L)
   )
   
   layout = layout_as_stemma(edgelist)
   expect_equal(layout, structure(c(-1, -2, -1, 0, 1, 1, -2.5, -1.5, 
                                      2, 1, -3, -1, 2, -8, -2, -4), 
                                  .Dim = c(8L, 2L)))
  
  
  # With hard to handle contamination
  # visualisation problem due to original 'layout_as_tree' function?
  edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", "A","A","D",
      "A", "B", "C", "E", "F","G","B",
      1,5,3,10,3,4,2), .Dim = c(7L, 3L)
  )
  
  layout = layout_as_stemma(edgelist)
  expect_equal(layout, structure(c(-1, -2, -1, 0, 1, 1, -2.5, -1.5, 2, 1, 0, -1, 2, 
                                   -8, -2, -3), .Dim = c(8L, 2L)))
  
  # With very hard to handle contamination
  
  edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", "A","A","E",
      "A", "B", "C", "E", "F","G","B",
      1,5,3,10,3,4,2), .Dim = c(7L, 3L)
  )
  #myNetwork = igraph::graph_from_edgelist(edgelist[,1:2], directed = TRUE)
  layout = layout_as_stemma(edgelist)
  #plot(myNetwork, layout = layout)
  expect_equal(layout,structure(c(-1, -2, -1, 0, 1, 1, -2.5, -1.5, 2, 1, -10, -1, 2, 
                                  -8, -2, -3), .Dim = c(8L, 2L)))
  
})

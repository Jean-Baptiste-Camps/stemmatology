context("PCC.reconstructModel")

test_that("Models are reconstructed correctly", {
  x = list(database = structure(
    c(1,0,1,1,1,1,1,1,0,1,2,2,2,1,1,0,0,3,2,1,NA,2,0,1,4,2,1,1,2,1,2,5,2,1,1),
    .Dim = c(7L, 5L),
    .Dimnames = list(
      c("VL1","VL2", "VL3", "VL4", "VL5", "VL6", "VL7"),
      c("A", "B", "C", "D","E"))
  ),
  groups = list(c("A", "B", "C"), c("D", "E")))
  
  results = list(
    oldDatabase = structure(
      c(
        1,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        1,
        2,
        2,
        2,
        1,
        1,
        0,
        0,
        3,
        2,
        1,
        NA,
        2,
        0,
        1,
        4,
        2,
        1,
        1,
        2,
        1,
        2,
        5,
        2,
        1,
        1,
        1,
        0,
        1,
        NA,
        2,
        1,
        1
      ),
      .Dim = c(7L, 6L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7"),
        c("A", "B", "C", "D", "E", "{ABC}")
      )
    ),
    database = structure(
      c(2,
        0, 1, 4, 2, 1, 1, 1, 0, 1, NA, 2, 1, 1),
      .Dim = c(7L, 2L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7"),
        c("D",
          "{ABC}")
      )
    ),
    edgelist = structure(
      c("{ABC}", "{ABC}", "{ABC}",
        "D", "A", "B", "C", "E"),
      .Dim = c(4L, 2L)
    ),
    models = list(
      structure(
        c(1,
          0, 1, NA, 2, 1, 1),
        .Dim = c(7L, 1L),
        .Dimnames = list(c(
          "VL1",
          "VL2", "VL3", "VL4", "VL5", "VL6", "VL7"
        ), "{ABC}")
      ),
      structure(
        c(2,
          NA, 1, NA, 2, 1, 1),
        .Dim = c(7L, 1L),
        .Dimnames = list(c(
          "VL1",
          "VL2", "VL3", "VL4", "VL5", "VL6", "VL7"
        ), "{DE}")
      )
    ),
    modelsByGroup = structure(
      c("{ABC}", "D"),
      .Dim = 1:2,
      .Dimnames = list("Models", c("ABC", "DE"))
    )
  )
  
  expect_equal(PCC.reconstructModel(x), results)
    
})

#TODO: add more tests, for the various options

##TODO: use following objects to implement more tests for all the PCC.Stemma functions
# x = matrix(
#   c(
#     1,0,1,1,1,1,1,
#     1,0,1,2,2,2,1,
#     1,0,0,3,2,1,NA,
#     2,0,1,4,2,1,1,
#     2,1,2,5,2,1,1
#   ), nrow = 7, ncol = 5, 
#   dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7"), 
#                   c("A","B","C","D","E")))
# x = PCC.disagreement(x)
# x = PCC.buildGroup(x)

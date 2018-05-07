context("PCC.reconstructModel")

# Testing including options omissionsAsReadings 
# and recoverNAs
test_that("Models are reconstructed correctly", {
  x = list(database = matrix(
    c(
      1,0,1,1,1,1,1,1,
      1,0,1,2,2,2,1,2,
      1,0,0,3,2,1,NA,3,
      2,0,1,4,NA,1,1,1,
      2,1,2,5,2,1,1,4
    ), nrow = 8, ncol = 5,
    dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7","VL8"),
                    c("A","B","C","D","E"))), 
    groups = list(c("A", "B", "C"), c("D", "E")))
  
  results = list(
    fullDatabase = structure(
      c(1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 2, 2, 1, 2, 1, 0, 0, 3, 2, 1, NA, 3, 2, 0, 1, 4, 2, 1, 1, 1, 2, 1, 2, 5, 2, 1, 1, 4, 1, 0, 1, NA, 2, 1, 1, 1), .Dim = c(8L, 6L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), c("A", "B", "C", "D", "E", "{ABC}"))), 
    database = structure(c(2, 0, 1, 4, 2, 1, 1, 1, 1, 0, 1, NA, 2, 1, 1, 1), .Dim = c(8L, 2L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), c("D", "{ABC}"))),
    edgelist = structure(
      c("{ABC}", "{ABC}", "{ABC}", "D", 
        "A", "B", "C", "E",
        "1","2","2","4"
        ), .Dim = c(4L, 3L)), 
    models = 
      matrix(c(1, 0, 1, NA, 2, 1, 1, 1,2, NA, 1, NA, 2, 1, 1, 1),
        nrow = 8, ncol = 2, 
        dimnames = 
          list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), 
               c("{ABC}","{DE}"))),
    modelsByGroup = structure(c("{ABC}", "D"), 
                    .Dim = 1:2, .Dimnames = list("Models", c("ABC", "DE"))))
    
  expect_equal(PCC.reconstructModel(x), results)

  results = list(
    fullDatabase = structure(
      c(1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 2, 2, 1, 2, 1, 0, 0, 3, 2, 1, NA, 3, 2, 0, 1, 4, 2, 1, 1, 1, 2, 1, 2, 5, 2, 1, 1, 4, 1, 0, 1, NA, 2, 1, 1, 1), 
      .Dim = c(8L, 6L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), c("A", "B", "C", "D", "E", "{ABC}"))), 
    database = structure(c(2, 0, 1, 4, 2, 1, 1, 1, 1, 0, 1, NA, 2, 1, 1, 1), .Dim = c(8L, 2L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), c("D", "{ABC}" ))), 
    edgelist = structure(
      c("{ABC}", "{ABC}", "{ABC}", "D", "A", "B", "C", "E",
        "1","2","2","4"
        ), .Dim = c(4L, 3L)), 
    models = 
      matrix(c(1, 0, 1, NA, 2, 1, 1, 1,2, 0, 1, NA, 2, 1, 1, 1), nrow=8, ncol=2, dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), c("{ABC}","{DE}"))), 
    modelsByGroup = structure(c("{ABC}", "D" ), .Dim = 1:2, .Dimnames = list("Models", c("ABC", "DE"))))
  
  expect_equal(PCC.reconstructModel(x, omissionsAsReadings = TRUE), results)
  
  result = expect_output(PCC.reconstructModel(x, recoverNAs = FALSE, verbose = TRUE))
  
  expect_equal(result$fullDatabase[5,4], as.double(NA))
  expect_equal(result$database[5,1], as.double(NA))
  expect_equal(result$models[5,2], 2)
})

#TODO: add more tests, for ask, verbose, etc.
# x = matrix(
#   c(
#     1,0,1,1,1,1,1,1,
#     1,0,1,2,2,2,1,2,
#     1,0,0,3,2,1,NA,3,
#     2,0,1,4,2,1,1,1,
#     2,1,2,5,2,1,1,4
#   ), nrow = 8, ncol = 5,
#   dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7","VL8"),
#                   c("A","B","C","D","E")))
# x = PCC.disagreement(x)
# x = PCC.buildGroup(x)

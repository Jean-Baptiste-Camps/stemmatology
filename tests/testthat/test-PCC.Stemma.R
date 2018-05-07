context("PCC.Stemma")

test_that("PCC.Stemma works properly", {
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
    fullDatabase = structure(
      c(1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 2, 2, 1, 2, 1,
        0, 0, 3, 2, 1, NA, 3, 2, 0, 1, 4, 2, 1, 1, 1, 2, 1, 2, 5, 2, 
        1, 1, 4, 1, 0, 1, NA, 2, 1, 1, 1, NA, 0, 1, 4, 2, 1, 1, 1), 
      .Dim = c(8L, 7L), 
      .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), 
                       c("A", "B", "C", "D", "E", "{ABC}", "{D{ABC}}"))),
    database = structure(
      c(NA, 0, 1, 4, 2, 1, 1, 1),
      .Dim = c(8L, 1L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"),
        "{D{ABC}}"
      )
    ),
    edgelist = structure(
      c(
        "{ABC}","{ABC}","{ABC}","D","{D{ABC}}","{D{ABC}}",
        "A","B","C","E","D","{ABC}",
        "1","2","2","4","0","0"
      ),
      .Dim = c(6L, 3L)
    ),
    models = structure(
        c(1, 0, 1, NA, 2, 1, 1, 1,
          2, NA, 1, NA, 2, 1, 1, 1,
          NA, 0, 1, 4, 2, 1, 1, 1
          ),
        .Dim = c(8L, 3L),
        .Dimnames = list(c(
          "VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"
        ), c("{ABC}", "{DE}","{D{ABC}}"))
      ),
    modelsByGroup = structure(
        c("{ABC}", "D","{D{ABC}}"),
        .Dim = c(1L, 3L),
        .Dimnames = list("Models", c("ABC", "DE","D{ABC}"))
      )
    )
  
  expect_equal(PCC.Stemma(x, ask = FALSE), results)
})

test_that("PCC.Stemma works properly when no group can be built", {
  # No group at first level
  x = matrix(
    c(
      1,1,1,0,
      1,2,1,1,
      1,2,2,2,
      2,1,1,1,
      2,1,1,2
    ), nrow = 4, ncol = 5,
    dimnames = list(c("VL1","VL2","VL3","VL4"),
                    c("A","B","C","D","E")))
  
  expect_equal(expect_message(PCC.Stemma(x, ask = FALSE)), NULL)
  
  # No group at second level with omissionsAsReadings to FALSE
  
  x = matrix(
    c(
      1,1,1,0,
      1,2,1,1,
      1,2,2,2,
      2,1,1,1,
      2,1,1,2,
      2,0,1,2
    ), nrow = 4, ncol = 6,
    dimnames = list(c("VL1","VL2","VL3","VL4"),
                    c("A","B","C","D","E","F")))
  
  results = structure(
    list(fullDatabase = matrix(
      c(
        1,1,1,0,
        1,2,1,1,
        1,2,2,2,
        2,1,1,1,
        2,1,1,2,
        2,0,1,2
      ), nrow = 4, ncol = 6,
      dimnames = list(c("VL1","VL2","VL3","VL4"),
                      c("A","B","C","D","E","F"))),
         database = 
           structure(c(1, 1, 1, 0, 1, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1, 2), 
            .Dim = 4:5, 
            .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), 
                             c("A", "B", "C", "D", "E"))), 
         edgelist = structure(c("E", "F", "1"), .Dim = c(1L,3L)), 
         models = structure(c(2, 1, 1, 2), .Dim = c(4L, 1L), .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), "{EF}")), 
         modelsByGroup = structure("E", .Dim = c(1L, 1L), .Dimnames = list("Models", "EF"))))
  
  expect_equal(expect_message(PCC.Stemma(x, ask = FALSE)), results)
  
})

#TODO: implement more tests to see if options are passed along properly

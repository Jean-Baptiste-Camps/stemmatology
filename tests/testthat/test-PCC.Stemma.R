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
    edgelist = structure(
      c(
        "{ABC}","{ABC}","{ABC}","D","{D{ABC}}","{D{ABC}}",
        "A","B","C","E","D","{ABC}"
      ),
      .Dim = c(6L, 2L)
    ),
    database = structure(
      c(NA, 0, 1, 4, 2, 1, 1, 1),
      .Dim = c(8L, 1L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"),
        "{D{ABC}}"
      )
    ),
    modelsGlobal = list(list(
      structure(
        c(1, 0, 1, NA, 2, 1, 1, 1),
        .Dim = c(8L, 1L),
        .Dimnames = list(c(
          "VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"
        ), "{ABC}")
      ),
      structure(
        c(2, NA, 1, NA, 2, 1, 1, 1),
        .Dim = c(8L, 1L),
        .Dimnames = list(c(
          "VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"
        ), "{DE}")
      )
    ), list(structure(
      c(NA, 0, 1, 4, 2, 1, 1, 1),
      .Dim = c(8L, 1L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"),
        "{D{ABC}}"
      )
    ))),
    modelsByGroupGlobal = list(
      structure(
        c("{ABC}", "D"),
        .Dim = 1:2,
        .Dimnames = list("Models", c("ABC", "DE"))
      ),
      structure(
        "{D{ABC}}",
        .Dim = c(1L, 1L),
        .Dimnames = list("Models", "D{ABC}")
      )
    )
  )
  
  expect_equal(PCC.Stemma(x, ask = FALSE), results)
})

#TODO: implement more tests to see if options are passed along properly

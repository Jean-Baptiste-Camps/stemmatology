context("PCC")

test_that("yields expected output on fournival", {
  # Skipping the following test on CRAN, as it will take too long
  skip_on_cran()
  data("fournival")
  expect_equal_to_reference(
    expect_output(
      expect_message(
        PCC(fournival, ask = FALSE, threshold = 0.06, verbose = FALSE)
        )
      ),
    file = "pccFourni.rds"
  )
})

# adding smaller / faster tests for CRAN
test_that("yields expected output on simple case (with no conflicts)", {
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
  expect_equal(expect_message(PCC(x, ask = FALSE, threshold = 0.06)), results)
})


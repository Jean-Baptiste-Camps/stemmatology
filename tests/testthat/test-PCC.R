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

test_that("yields expected output on simple case (with conflicts)", {
  x = matrix(data = c(
    1,1,1,2,2,
    0,0,0,0,1,
    1,1,0,1,2,
    1,2,3,4,5,
    1,2,2,NA,2,
    1,2,1,1,1,
    1,1,NA,1,1,
    1,2,3,1,4,
    1,1,3,2,2,
    1,3,1,2,2,
    3,1,1,2,2,
    1,2,2,2,1
  ), byrow = TRUE,
  ncol = 5,
  nrow = 12,
  dimnames = list(
    c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11", "VL12"),
    c("A","B","C","D","E")
  )
  )
  
  # With a threshold that will eliminate all conflicts
  results = list(
    fullDatabase = structure(
      c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 3,
        1, 0, 1, 2, 2, 2, 1, 2, 1, 3, 1,
        1, 0, 0, 3, 2, 1, NA, 3, 3, 1, 1,
        2, 0, 1, 4, 2, 1, 1, 1, 2, 2, 2, 
        2, 1, 2, 5, 2, 1, 1, 4, 2, 2, 2,
        1, 0, 1, NA, 2, 1, 1, 1, 1, 1, 1,
        NA, 0, 1, 4, 2, 1, 1, 1, NA, NA, NA), 
      .Dim = c(11L, 7L), 
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11"), 
        c("A", "B", "C", "D", "E", "{ABC}", "{D{ABC}}"))),
    database = structure(
      c(NA, 0, 1, 4, 2, 1, 1, 1, NA, NA, NA),
      .Dim = c(11L, 1L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11"),
        "{D{ABC}}"
      )
    ),
    edgelist = structure(
      c(
        "{ABC}","{ABC}","{ABC}","D","{D{ABC}}","{D{ABC}}",
        "A","B","C","E","D","{ABC}",
        "2","3","3","4","0","0"
      ),
      .Dim = c(6L, 3L)
    ),
    models = structure(
      c(1, 0, 1, NA, 2, 1, 1, 1, 1, 1, 1,
        2, NA, 1, NA, 2, 1, 1, 1, 2, 2, 2,
        NA, 0, 1, 4, 2, 1, 1, 1, NA, NA, NA
      ),
      .Dim = c(11L, 3L),
      .Dimnames = list(c(
        "VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11"
      ), c("{ABC}", "{DE}","{D{ABC}}"))
    ),
    modelsByGroup = structure(
      c("{ABC}", "D","{D{ABC}}"),
      .Dim = c(1L, 3L),
      .Dimnames = list("Models", c("ABC", "DE","D{ABC}"))
    )
  )
  expect_equal(expect_message(PCC(x, ask = FALSE, threshold = 0.6)), results)
  
  # With a threshold that lead to PCC.equipollent and two stemmata
  # First stemma
  result1 = results
  result1$fullDatabase = rbind(
    result1$fullDatabase, 
    c(NA, 2, 2, NA, NA, 2, 2)
    )
  rownames(result1$fullDatabase)[12] = "VL12"
  result1$database = rbind(
    result1$database, 
    2
  )
  rownames(result1$database)[12] = "VL12"
  result1$models = rbind(
    result1$models, 
    c(2, NA, 2)
  )
  rownames(result1$models)[12] = "VL12"
  
  # Second stemma
  result2 = list(
    fullDatabase = structure(
      c(NA, 0, 1, 1, 1, 1, 1, 1, NA, NA, 3, 1,
        1, 0, 1, 2, 2, 2, 1, 2, 1, 3, 1, 2,
        1, 0, 0, 3, 2, 1, NA, 3, 3, 1, 1, 2,
        1, 0, 1, 4, 2, 1, 1, 1, NA, NA, 2, 2, 
        NA, 1, 2, 5, 2, 1, 1, 4, NA, NA, 2, 1, 
        NA, NA, 1, NA, 2, 1, 1, 1, NA, NA, 2, 1, 
        1, 0, 1, NA, 2, 1, 1, NA, NA, NA, 1, 2), 
      .Dim = c(12L, 7L), 
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11", "VL12"), 
        c("A", "B", "C", "D", "E", "{AE}", "{BC}"))),
    database = structure(
      c(1, 0, 1, 4, 2, 1, 1, 1, NA, NA, 2, 2),
      .Dim = c(12L, 1L),
      .Dimnames = list(
        c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11", "VL12"),
        "D"
      )
    ),
    edgelist = structure(
      c(
        "{AE}","{AE}","{BC}","{BC}","D","D",
        "A","E","B","C","{AE}", "{BC}",
        "2","2","1","1","1","1"
      ),
      .Dim = c(6L, 3L)
    ),
    models = structure(
      c(NA, NA, 1, NA, 2, 1, 1, 1, NA, NA, 2, 1, 
        1, 0, 1, NA, 2, 1, 1, NA, NA, NA, 1, 2,
        1, 0, 1, 4, 2, 1, 1, 1, NA, NA, 2, 2
      ),
      .Dim = c(12L, 3L),
      .Dimnames = list(c(
        "VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11", "VL12"
      ), c("{AE}", "{BC}","{D{AE}{BC}}"))
    ),
    modelsByGroup = structure(
      c("{AE}", "{BC}","D"),
      .Dim = c(1L, 3L),
      .Dimnames = list("Models", c("AE", "BC","D{AE}{BC}"))
    )
  )
  
  # and then test
  results = list(result1, result2)  
  expect_equal(
    expect_message(expect_output(PCC(x, ask = FALSE, threshold = 2))),
    results
  )
  
})

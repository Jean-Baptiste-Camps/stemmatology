context("PCC.Exploratory")

test_that("yields expected output on fournival", {
  # Skipping the following test on CRAN, as it will take too long
  skip_on_cran()
  data("fournival")
  expect_equal_to_reference(expect_output(
    PCC.Exploratory(
      fournival,
      ask = FALSE,
      threshold = 0.06,
      verbose = FALSE
    )
  ),
  file = "equipollent_fourni_witD.rds")
})

test_that("does not fail when there are no conflicts at all in the tradition", {
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
    edgelist = structure(character(0), .Dim = c(0L,2L)), 
    conflictsTotal = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
          .Dim = c(8L, 2L), 
          .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8"), 
                           c("Number of conflicts", "Centrality index"))),
    database = x)
  class(results) = "pccConflicts"
  expect_equal(expect_message(PCC.Exploratory(x)), results)
  
})

test_that("user input is checked correctly", {
  
  x = data.frame(x = 1, y = 1:10)
  expect_error(PCC.Exploratory(x))
  
  x = matrix(1:10, nrow = 2)
  expect_error(PCC.Exploratory(x, ask = FALSE))
  
  #TODO: check for colnames / rownames
})

test_that("yields expected output on smaller case", {
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
    edgelist = 
      structure(character(0), .Dim = c(0L, 2L)), 
    conflictsTotal = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
        .Dim = c(11L, 2L), 
        .Dimnames = list(c("VL1", "VL2", "VL3", "VL4", "VL5", "VL6", "VL7", "VL8", "VL9", "VL10", "VL11"), 
                         c("Number of conflicts", "Centrality index"))), 
    database = x[-12,]#Removing the 12th VL
    )
  class(results) = 'pccConflicts'
  expect_equal(
    expect_message(PCC.Exploratory(x, ask = FALSE, threshold = 0.6)),
    results
  )
  
  # Now with a higher threshold, that will lead to the use of PCC equipollent
  db = PCC.conflicts(x)
  notInConflict = list(c("VL1", "VL9", "VL10"), "VL12")
  y = x
  y["VL12", c("D", "A", "E")] = NA
  z = x
  z[c("VL1","VL9", "VL10"), c("D", "A", "E")] = NA
  results = list(
    databases = list(y,z),
    notInConflict = notInConflict
  )
  class(results) = "pccEquipollent"
  expect_equal(
    expect_message(
      expect_output(
        PCC.Exploratory(x, ask = FALSE, threshold = 2)
        )
      ),
    results
    )
})

# Extended database to have conflicts
# and an potential equipollent database
# Repeat variation of VL1 at VL9-11
# to increase genealogical tendancy
# and then add a conflicting VL
# Expected config one:
# {ABC} (with virtual model)
# {DE} (with D as model)
#
# Expected config two
# {AE} (with virtual model)
# {BCD} (with D as model)
# -> not really:
# 1 severe disagr. BCD vs AE
# 1 deuxième, VL11, BC vs DE
# {BC} with virtual model
# {AE} with virtual model
#  D as archetype
#     A B  C  D E
# VL1 1 1  1  2 2
# VL2 0 0  0  0 1
# VL3 1 1  0  1 2
# VL4 1 2  3  4 5
# VL5 1 2  2 NA 2
# VL6 1 2  1  1 1
# VL7 1 1 NA  1 1
# VL8 1 2  3  1 4
# VL9 1 1  3  2 2
#VL10 1 3  1  2 2
#VL11 3 1  1  2 2
#VL12 1 2  2  2 1

# Details config two
#     A B  C  D E {AE} {BC}  {D{AE}{BC}}
# VL1 N 1  1  N N  NA   1       1
# VL2 0 0  0  0 1  NA   0       0
# VL3 1 1  0  1 2   1   1       1
# VL4 1 2  3  4 5  NA  NA       4
# VL5 1 2  2 NA 2   2   2       2
# VL6 1 2  1  1 1   1   1       1
# VL7 1 1 NA  1 1   1   1       1
# VL8 1 2  3  1 4   1  NA       1
# VL9 N 1  3  N N  NA  NA      NA
#VL10 N 3  1  N N  NA  NA      NA
#VL11 3 1  1  2 2   2   1       2
#VL12 1 2  2  2 1   1   2       2

#TODO: extend it to other exploratory functions !

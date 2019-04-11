context("PCC.disagreement")

test_that("incorrect input is spotted", {
  x = matrix(
    c("1","0","1","0","1","1","1","2","2","2","2","2","2",NA,"0","3",NA,"3","0","3"), 
    nrow = 4, ncol = 5, 
    dimnames = list(c("VL1","VL2","VL3","VL4"), c("A","B","C","D","E")))
  
  # Character matrix
  expect_error(PCC.disagreement(x))
  
  # Data-frame
  x = data.frame(x = 1, y = 1:10)
  expect_error(PCC.disagreement(x))
  
})

test_that("disagreements, agreements and omissions are correctly computed", {
  x = matrix(
    c(1,0,1,0,1,1,1,2,2,2,2,2,2,NA,0,3,NA,3,0,3), 
    nrow = 4, ncol = 5, 
    dimnames = list(c("VL1","VL2","VL3","VL4"), c("A","B","C","D","E")))
  results = list(
    database = structure(
      c(1, 0, 1, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, NA, 0, 3, NA, 3, 0, 3),
      .Dim = 4:5,
      .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), c("A", "B", "C", "D", "E"))
    ),
    severeDisagreement = structure(
      c(0, 0, 1, 1, NA, 0, 0, 1, 2, 1, 1, 1, 0, 1, 1, 1, 2, 1, 0, 0, NA, 1, 1, 0, 0),
      .Dim = c(5L, 5L),
      .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
    ),
    benignDisagreement = structure(
      c(0, 0, 1, 0, NA, 0, 0, 2, 0, 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, NA, 1, 1, 0, 0),
      .Dim = c(5L, 5L),
      .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
    ),
    agreements = structure(
      c(2, 2, 0, 0, NA, 2, 4, 1, 0, 0, 0, 1, 4, 1, 0, 0, 0, 1, 2, 1, NA, 0, 0, 1, 2),
      .Dim = c(5L, 5L),
      .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
    ),
    omissionsInCommon = structure(
      c(2,NA,NA,0,0,0,NA,NA,0,0,0,NA,NA,0,0,0,NA,NA,1,1,0,NA,NA,1,1),
      .Dim = c(5L, 5L),
      .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
    ),
    omissionsOriented = structure(
      c(0,NA,NA,1,1,2,NA,NA,1,1,2,NA,NA,1,1,1,NA,NA,0,0,2,NA,NA,0,0),
      .Dim = c(5L, 5L),
      .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
    ))
  expect_equal(PCC.disagreement(x), results)
})

# x = matrix(
#   c(1,0,1,0,1,1,1,1,2,2,2,2,2,2,2,2,NA,0,3,2,NA,3,0,3,NA), 
#   nrow = 5, ncol = 5, 
#   dimnames = list(c("VL1","VL2","VL3","VL4","VL5"), c("A","B","C","D","E")))

#With omissionsAsReadings to default FALSE
#      A B C D E
# VL1  1 1 2 2 NA
# VL2  0 1 2 NA 3
# VL3  1 1 2 0  0
# VL4  0 2 2 3 3
# 
# $severeDisagreement
# A B C D  E
# A  0 0 1 1 NA
# B  0 0 1 2  1
# C  1 1 0 1  1
# D  1 2 1 0  0
# E NA 1 1 0  0
# 
# $benignDisagreement
# A B C D  E
# A  0 0 1 0 NA
# B  0 0 2 0  1
# C  1 2 0 0  1
# D  0 0 0 0  0
# E NA 1 1 0  0
# 
# $agreements
# A B C D  E
# A  2 2 0 0 NA
# B  2 4 1 0  0
# C  0 1 4 1  0
# D  0 0 1 2  1
# E NA 0 0 1  2
# 
# $omissionsInCommon
# A  B  C  D  E
# A  2  0  0  0  0
# B NA NA NA NA NA
# C NA NA NA NA NA
# D  0  0  0  1  1
# E  0  0  0  1  1
# 
# $omissionsOriented
# A  B  C  D  E
# A  0  2  2  1  2
# B NA NA NA NA NA
# C NA NA NA NA NA
# D  1  1  1  0  0
# E  1  1  1  0  0


test_that("disagreements, agreements and omissions 
          are correctly computed with omissionsAsReadings", {
  x = matrix(
    c(1,0,1,0,1,1,1,2,2,2,2,2,2,NA,0,3,NA,3,0,3), 
    nrow = 4, ncol = 5, 
    dimnames = list(c("VL1","VL2","VL3","VL4"), c("A","B","C","D","E")))
  results = list(
      database = structure(
        c(1, 0, 1, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, NA, 0, 3, NA, 3, 0, 3),
        .Dim = 4:5,
        .Dimnames = list(c("VL1", "VL2", "VL3", "VL4"), c("A", "B", "C", "D", "E"))
      ),
      severeDisagreement = structure(
        c(0, 0, 1, 2, 1, 0, 0, 1, 3, 2, 1, 1, 0, 1, 1, 2, 3, 1, 0, 0, 1, 2, 1, 0, 0),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
      ),
      benignDisagreement = structure(
        c(0, 2, 3, 1, 2, 2, 0, 2, 0, 1, 3, 2, 0, 1, 2, 1, 0, 1, 0, 0, 2, 1, 2, 0, 0),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
      ),
      agreements = structure(
        c(4, 2, 0, 0, 0, 2, 4, 1, 0, 0, 0, 1, 4, 1, 0, 0, 0, 1, 3, 2, 0, 0, 0, 2, 3),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
      ),
      omissionsInCommon = structure(
        c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
      ),
      omissionsOriented = structure(
        c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
      )
    )
  expect_equal(PCC.disagreement(x, omissionsAsReadings = TRUE), results)
})
#With omissionsAsReadings = TRUE
# $database
# A B C  D  E
# VL1 1 1 2  2 NA
# VL2 0 1 2 NA  3
# VL3 1 1 2  0  0
# VL4 0 2 2  3  3
# 
# $severeDisagreement
# A B C D E
# A 0 0 1 2 1
# B 0 0 1 3 2
# C 1 1 0 1 1
# D 2 3 1 0 0
# E 1 2 1 0 0
# 
# $benignDisagreement
# A B C D E
# A 0 2 3 1 2
# B 2 0 2 0 1
# C 3 2 0 1 2
# D 1 0 1 0 0
# E 2 1 2 0 0
# 
# $agreements
# A B C D E
# A 4 2 0 0 0
# B 2 4 1 0 0
# C 0 1 4 1 0
# D 0 0 1 3 2
# E 0 0 0 2 3
# 
# $omissionsInCommon
# A  B  C  D  E
# A NA NA NA NA NA
# B NA NA NA NA NA
# C NA NA NA NA NA
# D NA NA NA NA NA
# E NA NA NA NA NA
# 
# $omissionsOriented
# A  B  C  D  E
# A NA NA NA NA NA
# B NA NA NA NA NA
# C NA NA NA NA NA
# D NA NA NA NA NA
# E NA NA NA NA NA

test_that("disagreements, agreements and omissions are correctly computed on a slightly larger case", {
  x= matrix(
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
    database = x,
    severeDisagreement = 
        structure(
          c(0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0),
          .Dim = c(5L, 5L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))), 
    benignDisagreement = structure(
        c(0, 4, 3, 1, 4, 4, 0, 3, 3, 4, 3, 3, 0, 2, 2, 1, 3, 2, 0, 3, 4, 4, 2, 3, 0), 
          .Dim = c(5L, 5L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))), 
    agreements = structure(
        c(7, 3, 2, 4, 2, 3, 7, 2, 2, 2, 2, 2, 5, 1, 2, 4, 2, 1, 6, 3, 2, 2, 2, 3, 8), 
        .Dim = c(5L, 5L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))), 
    omissionsInCommon = structure(
        c(1, 1, 1, 1, NA, 1, 1, 1, 1, NA, 1, 1, 2, 1, NA, 1, 1, 1, 1, NA, 0, 0, 0, 0, NA), 
        .Dim = c(5L, 5L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))), 
    omissionsOriented = structure(
        c(0, 0, 1, 0, NA, 0, 0, 1, 0, NA, 0, 0, 0, 0, NA, 0, 0, 1, 0, NA, 1, 1, 2, 1, NA),
        .Dim = c(5L, 5L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))))
  
  expect_equal(PCC.disagreement(x), results)
})
# TODO: the same with omissionsAsReadings?

# $database
#     A B  C  D E
# VL1 1 1  1  2 2
# VL2 0 0  0  0 1
# VL3 1 1  0  1 2
# VL4 1 2  3  4 5
# VL5 1 2  2 NA 2
# VL6 1 2  1  1 1
# VL7 1 1 NA  1 1
# VL8 1 2  3  1 4
# 
# $severeDisagreement
#   A B C D E
# A 0 0 0 1 1
# B 0 0 0 1 1
# C 0 0 0 1 1
# D 1 1 1 0 0
# E 1 1 1 0 0
# 
# $benignDisagreement
#   A B C D E
# A 0 4 3 1 4
# B 4 0 3 3 4
# C 3 3 0 2 2
# D 1 3 2 0 3
# E 4 4 2 3 0
# 
# $agreements
#   A B C D E
# A 7 3 2 4 2
# B 3 7 2 2 2
# C 2 2 5 1 2
# D 4 2 1 6 3
# E 2 2 2 3 8
# 
# $omissionsInCommon
#    A  B  C  D  E
# A  1  1  1  1  0
# B  1  1  1  1  0
# C  1  1  2  1  0
# D  1  1  1  1  0
# E NA NA NA NA NA
# 
# $omissionsOriented
#    A  B  C  D  E
# A  0  0  0  0  1
# B  0  0  0  0  1
# C  1  1  0  1  2
# D  0  0  0  0  1
# E NA NA NA NA NA


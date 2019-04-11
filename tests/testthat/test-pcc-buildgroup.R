context("PCC.buildGroup")

test_that("Groups are built correctly", {
  #First, we fake a PCC.disagreement object
  x = list(NULL)
  #TODO: do it in a cleaner way
  x$severeDisagreement = matrix(
    c(0,0,3,3,1,0,0,3,3,1,3,3,0,0,2,3,3,0,0,2,1,1,2,2,0),  
    nrow = 5,ncol = 5, 
    dimnames = list(c("A","B","C","D","E"),c("A","B","C","D","E")))
  results = PCC.buildGroup(x)
  expect_equal(c("A","B"), results$groups[[1]])
  expect_equal(c("C","D"), results$groups[[2]])
})

#   A B C D E
# A 0 0 3 3 1
# B 0 0 3 3 1
# C 3 3 0 0 2
# D 3 3 0 0 2
# E 2 2 2 2 2

test_that("Groups are built correctly with arbitrary limit", {
  x = list(NULL)
  x$severeDisagreement = matrix(
    c(0,0,3,3,1,0,0,3,3,1,3,3,0,0,2,3,3,0,0,2,1,1,2,2,0), 
    nrow = 5,ncol = 5, 
    dimnames = list(c("A","B","C","D","E"),c("A","B","C","D","E")))
  results = PCC.buildGroup(x, limit = 1)
  expect_equal(c("A","B","E"), results$groups[[1]])
  expect_equal(c("C","D"), results$groups[[2]])
})

test_that("Unexpected configurations are identified while building groups", {
  x = list(NULL)
  x$severeDisagreement = matrix(
    c(0,0,3,3,1,0,0,3,3,1,3,3,0,0,2,3,3,0,0,2,1,1,2,2,0), 
    nrow = 5,ncol = 5, 
    dimnames = list(c("A","B","C","D","E"),c("A","B","C","D","E")))
  results = expect_warning(PCC.buildGroup(x, limit = 2))
  expect_equal(2, length(results$groups))
  expect_equal(c("A","B","E"), results$groups[[1]])
  expect_equal(c("C","D","E"), results$groups[[2]])
})

test_that("NA are removed automatically in non interactive mode", {
  x = list(NULL)
  x$severeDisagreement = matrix(
    c(NA,NA,3,3,1,NA,NA,3,3,1,3,3,NA,NA,2,3,3,NA,NA,2,1,1,2,2,NA), 
    nrow = 5,ncol = 5, 
    dimnames = list(c("A","B","C","D","E"),c("A","B","C","D","E")))
  
  results =  PCC.buildGroup(x, ask = FALSE)
  expect_equal(c("A","B"), results$groups[[1]])
  expect_equal(c("C","D"), results$groups[[2]])
  
})

#TODO: test that the database is passed along? No great risk of bug.

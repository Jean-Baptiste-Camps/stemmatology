context("PCC.equipollent")

test_that("yields correct output on (cleaned) fournival", {
  # Skipping the following test on CRAN, as it will take too long
  skip_on_cran()
  data("fournival")
  y = PCC.conflicts(fournival)
  y = PCC.overconflicting(y, ask = FALSE, threshold = 0.06)
  y = PCC.elimination(y)
  y = PCC.conflicts(y)
  
  expect_equal_to_reference(
    PCC.equipollent(y, ask = FALSE, scope = "W", wits = "D"), 
    file = "equipollent_fourni_witD.rds")
  
  expect_equal_to_reference(
    expect_output(PCC.equipollent(y, ask = FALSE, scope = "T", verbose = TRUE)), 
    file = "equipollent_fourni_T.rds")
  
})


test_that("input is checked correctly", {
  # and now let's check for correct input
  x = c(0,1,2)
  expect_error(PCC.equipollent(x))
  expect_error(PCC.equipollent(y, ask = FALSE))
  expect_error(PCC.equipollent(y, ask = FALSE, scope = "A"))
  expect_error(PCC.equipollent(y, ask = FALSE, scope = "W"))
  expect_error(PCC.equipollent(y, ask = FALSE, scope = "W", wits = c('toto', 'tutu')))
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
  db = PCC.conflicts(x)
  notInConflict = list(c("VL1", "VL9", "VL10"), "VL12")
  
  # test for full tradition
  results = list(
    databases = list(
      x[-12,],
      x[c(-1,-9,-10),]
    ),
    notInConflict = notInConflict
  )
  class(results) = "pccEquipollent"
  expect_equal(
    PCC.equipollent(db, ask = FALSE, scope = "T"),
    results
  )
  
  # test for selected ms
  y = x
  y["VL12", c("D", "A")] = NA
  z = x
  z[c("VL1","VL9", "VL10"), c("D", "A")] = NA
  results$databases[[1]] = y
  results$databases[[2]] = z
  expect_equal(
    PCC.equipollent(db, ask = FALSE, scope = "W", wits = c("D","A")),
    results
  )
  
  # Check that there is an error if input with no row in edgelist
  db$edgelist = matrix(nrow = 0, ncol = 2)
  expect_error(PCC.equipollent(db, ask = FALSE, scope = "T"))
})

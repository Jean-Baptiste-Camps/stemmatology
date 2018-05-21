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





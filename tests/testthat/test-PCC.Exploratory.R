context("PCC.Exploratory")

test_that("yiels expected output on fournival", {
  
  expect_equal_to_reference(
    expect_output(PCC.Exploratory(fournival, ask = FALSE, threshold = 0.06, verbose = FALSE)),
    file = "equipollent_fourni_witD.rds"
  )
  
})

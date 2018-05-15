context("PCC.Exploratory")

test_that("yields expected output on fournival", {
  data("fournival")
  expect_equal_to_reference(
    expect_output(PCC.Exploratory(fournival, ask = FALSE, threshold = 0.06, verbose = FALSE)),
    file = "equipollent_fourni_witD.rds"
  )
  
})
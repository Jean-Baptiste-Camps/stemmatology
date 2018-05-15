context("PCC")

test_that("yiels expected output on fournival", {
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

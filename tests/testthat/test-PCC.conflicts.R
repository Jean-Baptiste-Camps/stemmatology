context("PCC.conflicts")
#TODO: check fournival output
test_that("PCC.conflicts correct output on Fournival", {
  data("fournival")
  myConflicts = PCC.conflicts(fournival)
  expect_equal_to_reference(myConflicts, file="myConflicts.rds")
})

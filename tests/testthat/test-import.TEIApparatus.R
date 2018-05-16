context("import.TEIApparatus")

test_that("yields correct output on Yvain", {
  
  # Default setting
  
  results = matrix(data = c(1, NA, NA, 1, 1, 0, NA, 1, NA, 1, 1, 1), 
                   ncol = 12L, 
                   dimnames = list("VL_3686.2.1", 
                    c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S", "V")))
  
  expect_equal(import.TEIApparatus(file = "Yvain.xml"), results)
  
  # All types
  results = matrix(data = c(1, 1, 2, 1, 1, 0, 0, 2, 2, NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 0, 2, 1, 1, 
                        0, 2, 2, 1, 2, NA, 2, 1, 1, 0, 1, 2, 1, 1, 1, 2, 0, 1, 1, 0, 
                        1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 2, 1, 2, 0, 0, 
                        2, 1, 1, 1, 1, NA, 1, 0, 0, 1, 2, 1, 1, 2, 1, 1, 0, 0, 1, 1, 
                        1, 1, 2, 1, 1, 0, 0, 2, 2, 1, 1, 2, 1, 1, 0, 0, 1, 1), 
                   nrow = 9, ncol = 12L, 
                   dimnames = list(
                     c("VL_3686.1", "VL_3686.1.1", "VL_3686.2", "VL_3686.2.1", "VL_3686.3", "VL_3686.4", "VL_3686.5", "8", "9"), 
                     c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S","V")))
  
  expect_equal(import.TEIApparatus(file = "Yvain.xml", appTypes = NULL), results)
  
  # A different selection
  results = matrix(data = 
                c(1, 0, 0, 2, NA, NA, NA, NA, NA, NA, NA, NA, 1, 0, 
                  2, 2, 2, 0, 1, 2, 1, 1, 0, 1, NA, NA, NA, NA, 1, 0, 0, 2, 1, 
                  0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 1), 
                nrow = 4, ncol = 12, 
                dimnames = list(
                  c("VL_3686.1", "VL_3686.4", "VL_3686.5", "4"), 
                  c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S", "V")))
  
  expect_equal(
    import.TEIApparatus(
      file = "Yvain.xml", appTypes = c('synonymism', 'functionWord')), 
    results)
  
  # types with no app selectected
  results = matrix(nrow = 0, ncol = 12, 
              dimnames = list(character(0), 
                    c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S", "V")))
  expect_equal(import.TEIApparatus(file = "Yvain.xml", appTypes = c('')), results)
})

test_that("can be imported from URL", {
  results = matrix(data = c(1, NA, NA, 1, 1, 0, NA, 1, NA, 1, 1, 1), 
                   ncol = 12L, 
                   dimnames = list("VL_3686.2.1", 
                                   c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S", "V")))
  expect_equal(
    import.TEIApparatus(
      #TODO: change it to the link to this repo?
      file = "https://github.com/Jean-Baptiste-Camps/stemmatology-utils/raw/master/test/Yvain.xml"),
    results
    )
})

#TODO: test for import with no proper ns or other ns
# test_that("works with ns other than tei (collatex)", {
#   
# })

test_that("works with alternative readings and imbrication", {
  results = matrix(data = 
              c("1", "2", NA, NA, NA, NA, NA, NA, NA, "1", "2", NA, 
              "1", "2", NA, "0", "1,2", "1,2", NA, NA, NA, "1", NA, NA, NA, 
              "1", "1", "1", "2", NA, "1", "2", NA, "1", "1", "2"), 
              nrow = 3, ncol = 12L, 
              dimnames = list(
                c("VL_3686.2.1", "VL_3686.3.1", "VL_3686.3.1.1"),
                c("A", "An", "Br", "F", "G", "H", "L", "M", "P", "R", "S", "V")))
  expect_equal(import.TEIApparatus(file = "Yvain2.xml"), results)
  #TODO: more tests for this mode?
})

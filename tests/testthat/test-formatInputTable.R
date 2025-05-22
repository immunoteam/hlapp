test_that("recogniseInputFormat() passed", {
 
    expect_equal(recogniseInputFormat(dummy_input1), "cohort")
    expect_equal(recogniseInputFormat(dummy_input2), "standard")
    expect_error(recogniseInputFormat(test_HLA_table1), "Unrecognised")

})



test_that("Cohort input format is recognised", {
  
  expect_equal(formatInputTable(dummy_input1), test_HLA_table1) # test if Cohort format is formatted
  expect_equal(formatInputTable(dummy_input2), test_HLA_table1) # test if standard format is formatted
  
})

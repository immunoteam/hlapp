test_that("readInputFile() reads files correctly", {
  
  df <- dummy_input1
    
  file_csv <- list(name = "test.csv",
                   datapath = tempfile() )
  file_tsv <- list(name = "test.tsv",
                   datapath = tempfile() )

  write.csv(df, file_csv$datapath, row.names = FALSE)
  write.table(df, file_tsv$datapath, sep = "\t", row.names = FALSE)
  
  expect_equal(readInputFile(file_csv), df)
  expect_equal(readInputFile(file_tsv), df)
  
})

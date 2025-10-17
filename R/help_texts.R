
helptext <- c(

  contact = "If you have any comments, questions or run into any issues while using the app, please contact koppy.gergo@gmail.com",
  input_file = paste("Upload your .tsv, .csv or .xlsx file with patient HLA genotype data",
                     "One row should only contain denotype information of one person. If there are data for more patients in one row, makes sure that you select the columns with one persons data.",
                     "HLA genotype of the patients can be in separate columns for each HLA allele, or if more alleles are within a cell, make sure they are separated with commas \", \"",
                     sep = "\n"),
  input_idcol = "Please select the column with patient identification. The identifiers should be unique for each row.",
  input_hlacol = paste("Select the columns that contain HLA genotype data, you want to analise.", 
                       "If there are genotype data for multiple patients in a row, make sure to select columns that only belong to one person, or consider submitting your data in another format", 
                       sep = "\n"),
  input_outcol = "Please select, what promiscuities you want in your report. You can calculate the promiscuity fore each allele / complex, or a mean promiscuity for each HLA loci."
)

usethis::use_data(helptext, overwrite = T)
rm(helptext)
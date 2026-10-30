# messages and display texts for the application

texts <- list(
  main_title = "HLA Promiscuity tool",

  # login UI
  login_title = "To acces the tool, please enter your credentials!",
  login_footer = "If you want to get access, please contact koppany.gergely@brc.hu",

  # App ui
  # main

  # upload modul
  upload_title = "1. Upload file",
  upload_main = "Upload your file with HLA genotypes. File should only contain a table with column names.",
  upload_label = "Upload your file with HLA genotypes", # not used
  upload_tooltip = ".xlsx, .xlx, .txt, .tsv or .csv file containing a table.\nThe file should not contain any additional lines, only the table.\nData for one patient/sample have to be stored in one row. ",
  upload_feedback_pass = "Table read succesfully.",
  upload_feedback_fail = "ERROR: Could not read table. Make sure your file only contains a table and no additional lines.",

  # select modul
  select_title = "2. Select input columns",
  select_main = "Detected columns that potentially contain sample IDs and HLA genotype data. Please select the columns you want to use.",
  select_id = "Select column of sample IDs:",
  select_id_tooltip = "IDs should be unique for each sample",
  select_id_pass = "Column(s) identified as potential ID column:",
  select_id_fail = "ERROR: Could not detect a potential ID column. (Column of unique, non-HLA genotype values)",
  select_hla_pass = "Column(s) identified as potential HLA genotype columns:",
  select_hla_fail = "ERROR: Could not detect any columns with recognised HLA allele data",
  select_hla = "Select columns of HLA data you want to analyze.",
  select_hla_tooltip = "In each row, selected columns should only contain HLA data of one sample/patient.\n One column can contain multiple alleles, separated with a comma.",
  select_check_pass = "",
  select_check_fail = "To proceed, please select at least one colum.",
  # result modul
  result_title = "3. Choose output",
  result_main = "Select the promiscuity values you want to be calculated",
  results_avail_pass = "Recognised HLA loci in the selected columns:",
  results_avail_fail = "ERROR: No HLA molecules were recognized in the selected columns",
  app_footer = "If you experience any issues, or if you want to give feedback on this software, please visit https://github.com/immunoteam/hlapp"
)

#' function to read input data from file to data frame
#' 
#' @param file input file object
#' 
#' @return the uploaded file as a data frame

readInputFile <-function(file){
  ext <- tools::file_ext(file$name)
  switch (ext,
          tsv = read.delim(file$datapath ),
          csv = read.csv(file$datapath), 
          xlsx = as.data.frame(readxl::read_excel(file$datapath)),
          validate("Invalid file. Must be a .csv, .tsv or .xlsx")
  )
}
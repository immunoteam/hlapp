# fuctions for shiny app

# get data frame from Area Input
dfFromArea <- function(Area){
  split_list <- strsplit(Area, split = "\n") |> 
    unlist() |>
    strsplit(split = ",") 
  
  res <- as.data.frame(do.call(rbind, split_list))
  return(res)
}

# get data.frame from file.input
dfFromFile <- function(File){
  file <- File
  input.df <- read.delim(file$datapath, sep = "\t") # this only works on local, i whould be accessed in another way
}

# function to respond to input
textResponseToInput <- function(file, area){
    if (is.null(file)) { 
      out <- "No file input, detected. Text input is used detected"
      } 
    else  {
      out <- "File input detected"
    } 
  return(out)
}


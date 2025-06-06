#' function to respond to if file input is uploaded/received
#' 
#' @param file input$file 
textResponseToInput <- function(file){
  if (is.null(file)) { 
    out <- "No file input, detected. Text input is used detected"
  } 
  else  {
    out <- "File input detected"
  } 
  return(out)
}



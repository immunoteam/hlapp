# subMotifCompFunctions

getCompPlot <- function(allele) {
  file_name <- paste0(allele, ".svg")

  shiny::tags$img(
    src = file.path("img/motif", file_name),
    style = "width:100%; max-width:900px;"
  )
}

getSubmotifText <- function(allele) {
  subm_text <- submotif_text[[allele]]

  if (length(subm_text) > 0) {
    cat(subm_text, sep = "\n")
  } else {
    "Not available"
  }
}

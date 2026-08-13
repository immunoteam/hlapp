#download functions

downloadFileName <- function(input, name, extension) {
  paste(name, "_",input$allele, "_cluster_", input$cluster, extension, sep = "")
}


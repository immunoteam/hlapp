#download functions

downloadFileName <- function(input, name, extension) {
  paste(name, "_",input$allele, "_cluster_", input$cluster, extension, sep = "")
}

makePng<- function(file, input, plotobject, method = "motifStack") {

  png(file)

  if(method == "motifStack"){
    motifStack::plot(plotobject)
  } else {
    plot(plotobject)
  }

  dev.off()
}

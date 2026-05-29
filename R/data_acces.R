# data acces functions

read_allele <- function(allele){
  
  file_name <- paste0("allele_", allele, ".rds")
  path <- system.file("extdata", "allele", file_name, package = "promiscuityApp")
#  message("allele: ", path)
  readRDS(path)
}

read_cluster <- function(type, allele, cluster) {
  
  file_name <- paste0( "cluster_", gsub("HLA_", "", type), "_", allele,  "_", cluster, ".rds")
  path <- system.file("extdata", "cluster", file_name, package = "promiscuityApp")
  message("cluster: ", path)
  
  readRDS(path)
}


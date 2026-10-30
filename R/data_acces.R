# data acces functions

read_allele <- function(allele) {
  file_name <- paste0("allele_", allele, ".rds")
  path <- file.path("data", "allele", file_name)
  #  message("allele: ", path)
  readRDS(path)
}

read_cluster <- function(type, allele, cluster) {
  file_name <- paste0("cluster_", gsub("HLA_", "", type), "_", allele, "_", cluster, ".rds")
  path <- file.path("data", "cluster", file_name)
  message("cluster: ", path)

  readRDS(path)
}

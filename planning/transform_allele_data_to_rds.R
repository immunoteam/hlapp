# transform allele and cluster .rda objects to .rds files
# alleles
allelefiles <- list.files("data/allele", full.names = T, recursive = T, pattern = ".rda")

for (file in allelefiles) {
  
  allele <- gsub("data/allele/|\\.rda", "", file)
  
  load(file) # load allele data to memory
  
  #save as rds
  object <- get(allele) 
  
  object_main <- list(nmds = object$nmds, clusters = object$clusters)
  object_motif <- object$motif
  
  saveRDS(object_main, file = gsub("rda", "rds", file))
  saveRDS(object_motif, file = gsub("rda", "rds", gsub("allele_", "allele_motif_", file)))
  
  unlink(file)
}

rm(list = ls())

# clusters
clusterfiles <- list.files("data/cluster", full.names = T, recursive = T, pattern = ".rda")

for (file in clusterfiles) {
  
  cluster <- gsub("data/cluster/|\\.rda", "", file)
  
  load(file) # load allele data to memory
  
  #save as rds
  object <- get(cluster) 
  
  object_main <- object[-1]
  object_motif <- object$motif
  
  saveRDS(object_main, file = gsub("rda", "rds", file))
  saveRDS(object_motif, file = gsub("rda", "rds", gsub("cluster_", "cluster_motif_", file)))
  
  unlink(file)
}

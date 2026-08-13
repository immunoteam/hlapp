submotifs_i <- readRDS("test_data/submotifresults_min_I.rds")
submotifs_ii <- readRDS("test_data/submotifresults_min_II.rds")

#alleles <- names(submotifs_i)
alleles <- names(submotifs_ii)

for (allele in alleles) {
  
   #allele_obj <- submotifs_i[[allele]]
  allele_obj <- submotifs_ii[[allele]]
  
  cluster_names <- gsub("_logo", "", grep("logo", names(allele_obj), value = T))
  
  for (cluster in cluster_names){
    motif_to_save <- allele_obj[[paste0(cluster, "_logo")]]
    message(allele, " , ", cluster)
    saveRDS(motif_to_save, file.path("data", "cluster_motif", paste0(allele, "_", cluster, "_motif.rds")))
  }
    
}

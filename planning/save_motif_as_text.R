extractMotifMat <- function(file_rds) {
  motif <- readRDS(file_rds)

  motif_txt <- motif$motif$mat
  colnames(motif_txt) <- paste0("pos_", 1:9)

  write.table(file = gsub("rds", "txt", file_rds), x = motif_txt, quote = F)
}

files <- list.files(path = "data/cluster_motif", pattern = "rds", full.names = T, recursive = T)

lapply(files, extractMotifMat)

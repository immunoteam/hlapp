# functions

getAvailableClusters <- function(submotif_results, allele_data,  allele){

  allele_clusters <- names(submotif_results[[allele]])
  logos <- grep("cluster_[0-9]+_logo", allele_clusters, value = TRUE)
  clusters <- gsub("cluster_|_logo", "", logos)

  # Klaszterek és azok peptid-száma
  cluster_peptide_counts <- sapply(clusters, function(cluster) {
    cluster_info <- allele_data[[allele]]$clusters
    sum(cluster_info$Peptide_Cluster == as.numeric(cluster))
  })

  # Klaszterek rendezése a peptid-szám alapján
  clusters_sorted <- clusters[order(cluster_peptide_counts)]

  return(clusters_sorted)
}


makeNmdsPlot <- function(allele_data, allele, cluster){

  nmds_df <- as.data.frame(allele_data$nmds)

  cluster_selected <- as.numeric(cluster)
  highlighted <- nmds_df$cluster == cluster_selected

  col <- ifelse(highlighted, "seagreen", "grey80")
  pch <- ifelse(highlighted, 21, 16)

  p <- plot(nmds_df$V1, nmds_df$V2,
       col = col, pch = pch, bg = col,
       xlab = "NMDS1", ylab = "NMDS2",
       main = paste("NMDS plot -", allele))

  return(p)
}

getEntropyValues <- function(allele, cluster, submotif_results){
  cluster_name <- paste0("cluster_", cluster, "_entropy")
  entropy_values <- submotif_results[[allele]][[cluster_name]]$molecularEntropy$H
}

getRedLine <- function(allele, cluster, submotif_results){
  cluster_name <- paste0("cluster_", cluster, "_entropy")
  entropy_values <- submotif_results[[allele]][[cluster_name]]$red_line
}


getClusterPeptides <- function(cluster_info, type, cluster) {

  if (type == "I") {
    cluster_info$Peptide[cluster_info$Peptide_Cluster == as.numeric(cluster)]
  } else { # if type == "II"
    cluster_info$Core[cluster_info$Peptide_Cluster == as.numeric(cluster)]
  }

}

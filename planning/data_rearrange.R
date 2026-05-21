
## HLA_i
# Allele motifs, and allele data
load("data/hla_i_allele_data.rda")

# keep only used data
allele_i <- lapply(hla_i_allele_data, function(x){
  motif <- x$motif
  
  nmds <- as.data.frame(x$nmds)
  nmds$cluster <- x$clusters$Peptide_Cluster
  
  clusters <- x$clusters |> dplyr::count(Peptide_Cluster) |> 
    dplyr::rename(Cluster = Peptide_Cluster, Peptide_n = n) |> 
    dplyr::arrange(desc(Peptide_n)) |>
    as.data.frame()

  return(list(motif = motif, nmds = nmds, clusters = clusters))
})
names(allele_i) <- paste0("allele_", names(allele_i))

# submitif, cluster data
load("data/hla_i_submotifresults.rda")

# keep only used data
submotif_i <- lapply(hla_i_submotifresults, function(x){
  
  cluster_names <- gsub("cluster_|_logo", "", grep( "logo", names(x), value = T)) 
  allele_name <- gsub("\nMS.+", "", x$cluster_1_logo$title )
  
  cluster_list <- lapply(cluster_names, function(clus){
    motif <- x[[paste0("cluster_", clus, "_logo")]][["motif"]]
    title <- x[[paste0("cluster_", clus, "_logo")]][["title"]]
    peptides <- x[[paste0("cluster_", clus, "_entropy")]][["peptides"]]
    entropy <- x[[paste0("cluster_", clus, "_entropy")]][["molecularEntropy"]]
    red_line <- x[[paste0("cluster_", clus, "_entropy")]][["red_line"]]
    
    return(list(motif = motif, title = title, peptides = peptides, entropy = entropy, red_line = red_line))
  })
  
  names(cluster_list) <- paste0("cluster_I_", allele_name, "_", cluster_names)
  
  return(cluster_list)
})
# remove top level of list 
submotif_i <- purrr::list_flatten(submotif_i)
names(submotif_i) <- gsub("^.+_cluster", "cluster", names(submotif_i))


# HLA II
# allele_data 
load("data/hla_ii_allele_data.rda")

# keep only used data
allele_ii <- lapply(hla_ii_allele_data, function(x){
  motif <- x$motif
  nmds <- as.data.frame(x$nmds)
  nmds$cluster <- x$clusters$Peptide_Cluster
  
  clusters <- x$clusters |> dplyr::count(Peptide_Cluster) |> 
    dplyr::rename(Cluster = Peptide_Cluster, Peptide_n = n) |> 
    as.data.frame() |> 
    dplyr::arrange(desc(Peptide_n))
  
  return(list(motif = motif, nmds = nmds, clusters = clusters))
})
names(allele_ii) <- paste0("allele_", names(allele_ii))


# submitif, cluster data
load("data/hla_ii_submotifresults.rda")

# keep only used data
submotif_ii <- lapply(hla_ii_submotifresults, function(x){
  
  cluster_names <- gsub("cluster_|_logo", "", grep( "logo", names(x), value = T)) 
  allele_name <- gsub("\nMS.+", "", x$cluster_1_logo$title )
  
  cluster_list <- lapply(cluster_names, function(clus){
    motif <- x[[paste0("cluster_", clus, "_logo")]][["motif"]]
    title <- x[[paste0("cluster_", clus, "_logo")]][["title"]]
    peptides <- x[[paste0("cluster_", clus, "_entropy")]][["peptides"]]
    entropy <- x[[paste0("cluster_", clus, "_entropy")]][["molecularEntropy"]]
    red_line <- x[[paste0("cluster_", clus, "_entropy")]][["red_line"]]
    
    return(list(motif = motif, title = title, peptides = peptides, entropy = entropy, red_line = red_line))
  })
  
  names(cluster_list) <- paste0("cluster_II_", allele_name, "_", cluster_names)
  
  return(cluster_list)
})
# remove top level of list 
submotif_ii <- purrr::list_flatten(submotif_ii)
names(submotif_ii) <- gsub("^.+_cluster", "cluster", names(submotif_ii))


# Save elements of lists as unique .rda objects
saveListElements <- function(x){
  
  listnames <- names(x)
  
  for(i in listnames){
    filename = paste0("data/", i, ".rda")
    #message(filename)
    assign(i, value = x[[i]])
    save(list = i, file = filename)
  }
}

saveListElements(allele_i)
saveListElements(allele_ii)
saveListElements(submotif_i)
saveListElements(submotif_ii)


#save list of ellele names
allele_names <- list(HLA_I = gsub("allele_", "", names(allele_i)), 
                     HLA_II = gsub("allele_", "", names(allele_ii))
                     ) 

save(allele_names, file ="data/allele_names.rda")

## test loading all
files <- list.files("data/motifs", full.names = T)

for(file in files){
  load(file)
}



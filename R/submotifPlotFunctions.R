# Plotting functions
makeNmdsPlot <- function(allele_data, allele, cluster) {
  nmds_df <- as.data.frame(allele_data$nmds)

  cluster_selected <- as.numeric(cluster)
  highlighted <- nmds_df$cluster == cluster_selected

  col <- ifelse(highlighted, "seagreen", "grey80")
  pch <- ifelse(highlighted, 21, 16)

  p <- plot(nmds_df$V1, nmds_df$V2,
    col = col, pch = pch, bg = col,
    xlab = "NMDS1", ylab = "NMDS2",
    main = paste("NMDS plot -", allele)
  )

  return(p)
}



# fnction to generate enetropy plot form entropy values
makeEntropyPlot <- function(entropy_values){
  #Egységes grafikai beállítások ----
  axis_text_size <- 1.2
  axis_title_size <- 1.2
  
  positions <- seq_along(entropy_values)
  
  #Y tengely beosztása, 0.1 hozzáadásával
  y_breaks <- sort(unique(c(
    0.1,
    seq(
      0,
      ceiling(max(entropy_values, na.rm = TRUE)),
      by = 0.5
    )
  )))
  
  
  #Grafikai beállítások az entrópiaábrához
  par(
    family = "sans",
    font.axis = 1,
    font.lab = 1,
    cex.axis = axis_text_size,
    cex.lab = axis_title_size
  )
  
  plot(
    positions,
    entropy_values,
    type = "o",
    col = "darkgreen",
    pch = 16,
    lwd = 1.5,
    
    xlab = "Position",
    ylab = "Entropy",
    main = "",
    
    xaxt = "n",
    yaxt = "n",
    
    xlim = range(positions),
    ylim = c(0, max(y_breaks)),
    
    cex.lab = axis_title_size,
    cex.axis = axis_text_size,
    
    font.lab = 1,
    font.axis = 1
  )
  
  #X tengely: minden pozíció
  axis(
    side = 1,
    at = positions,
    labels = positions,
    cex.axis = axis_text_size,
    font = 1
  )
  
  #Y tengely beosztása 0.1-es lépésközökkel
  y_breaks <- seq(
    0,
    ceiling(max(entropy_values, na.rm = TRUE) * 10) / 10,
    by = 0.1
  )
  
  #Y tengely megjelenítése
  axis(
    side = 2,
    at = y_breaks,
    labels = sprintf("%.1f", y_breaks),
    las = 1,
    cex.axis = axis_text_size,
    font = 1
  )
  #Piros szaggatott vonal 0.1-nél
  abline(
    h = 0.1,
    col = "red",
    lty = 2,
    lwd = 1.5
  )
  
}



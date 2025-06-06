middle_table = read.csv("planning/gergo/data/middle_table.csv")
load("Anna/promiscuity_hlai_imm")
p1 = promiscuity
load("planning/gergo/data/promiscuity_gfeller_100.RData")
p2 = promiscuity

promiscuity = c(p1, p2)


prom_df = NULL
prom_all = NULL

select_promiscuity = function(gene){
  if(startsWith(gene, "D")) n = 4 else n = 2
  prom = matrix(NA, nrow = length(unique(middle_table$ID)), ncol = n)
  rownames(prom) = unique(middle_table$ID)
  colnames(prom) = paste0(gene, 1:n)
  for(id in unique(middle_table$ID)){
    table = subset(middle_table, middle_table$ID == id & middle_table$Locus == gene)
    prom[id,] = table$Promiscuity
    if(gene == "DQ"){
      prom[id,table$forbidden_DQ == "forbidden!"] = "forbidden"
    }
  }
  return(prom)
}


if(("Ai" %in% input$type)){
  prom_df = cbind(prom_df, select_promiscuity("A"))
}

if(("Bi" %in% input$type)){
  prom_df = cbind(prom_df, select_promiscuity("B"))
}

if(("Ci" %in% input$type)){
  prom_df = cbind(prom_df, select_promiscuity("C"))
}



mean_promiscuity = function(genes){
  prom = rep(NA, length(unique(middle_table$ID)))
  names(prom) = unique(middle_table$ID)
  for(id in unique(middle_table$ID)){
    table = subset(middle_table, middle_table$ID == id & middle_table$Locus %in% genes)
    table = subset(table, table$forbidden_DQ != "forbidden!")
    prom[id] = mean(table$Promiscuity, na.rm = input$NA)
  }
  return(prom)
}



if(("HLA-A (average)" %in% input$type)){
  prom_df = cbind(prom_df, A = mean_promiscuity("A"))
}
if(("HLA-B (average)" %in% input$type)){
  prom_df = cbind(prom_df, B = mean_promiscuity("B"))
}
if(("HLA-C (average)" %in% input$type)){
  prom_df = cbind(prom_df, C = mean_promiscuity("C"))
}
if(("all HLA-I (average)" %in% input$type)){
  prom_df = cbind(prom_df, ABC = mean_promiscuity(c("A", "B", "C")))
}
prom_df = round(prom_df, 4)

prom_all = cbind(A = mean_promiscuity("A"), B = mean_promiscuity("B"), 
                 C = mean_promiscuity("C"), ABC = mean_promiscuity(c("A", "B", "C")))

# dummy tables

# output
dummy.output <- 
  data.frame(ID = c("1", "2"),
             Prom_A = c(2, 2.3), 
             Prom_B = c(4, 2.4), 
             Prom_C = c(5, 2.67), 
             Prom_DP = c(3.2, 2.82), 
             Prom_DQ = c(1.6, 3.4), 
             Prom_DR = c(2.4, 3.13), 
             Prom_GT_I = c(3.7, 2.3), 
             Prom_GT_II = c(2, 1.89)
  )
# dummy_inputű
dummy_file <- file.path("data", "dummy.txt")
write.table(dummy.output, dummy_file, sep = "\t", row.names = F)

# dummy input

dummy.input <- read.delim(file.path("data", "dummyinput.tsv"))

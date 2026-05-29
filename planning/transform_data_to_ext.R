
data_files <- list.files("data", pattern = "rda") 

for (file in data_files) {
  load(file.path("data", file))
  
  obj <- get(gsub(".rda", "", file))
  
  saveRDS(obj, file = file.path("inst", "extdata", gsub("rda", "rds", file)))
}

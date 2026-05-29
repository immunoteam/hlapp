
data_files <- list.files("data", pattern = "rda") 

for (file in data_files) {
  load(file.path("data", file))
  
  obj <- get(gsub(".rda", "", file))
  
  saveRDS(obj, file = file.path("inst", "extdata", gsub("rda", "rds", file)))
}


# also add to sysdata.rda

usethis::use_data(
  promiscuity,
  forbidden_DQ_combinations,
  allele_names,
  internal = TRUE,
  overwrite = TRUE
)

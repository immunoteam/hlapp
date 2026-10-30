# rearrange FED Data - FED data will be arranged in the same way as HED data, so the same functions can be used on them

func_div_data <- lapply(fd_matrix, function(x) {
  x |>
    as.data.frame() |>
    tibble::rownames_to_column("Allele1") |>
    pivot_longer(cols = !all_of("Allele1"), names_to = "Allele2", values_to = "FuncDiv")
})

func_div_data <- Reduce(rbind, func_div_data)
usethis::use_data(func_div_data, overwrite = T)

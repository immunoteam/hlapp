load("evolutionary_distances")

rows_to_keep <- gsub("\\d", "", evolutionary_distances$Allele1) == gsub("\\d", "", evolutionary_distances$Allele2)

evolutionary_distances <- evolutionary_distances[rows_to_keep, ]

usethis::use_data(evolutionary_distances)

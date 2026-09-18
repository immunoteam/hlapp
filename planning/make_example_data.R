readxl::read_excel("test_data/dummyinput/example_data.xlsx") -> x

colnames(x) <- gsub("R_", "", colnames(x))

x <- x |>
  mutate(across(where(is.character), ~gsub("P$", "", .x) )) 
  

y <- formatInputTable(x, hla.cols = colnames(x)[-1]) |> 
  mutate(class = ifelse(substr(HLA,1,1) == "D", "II", "I")) |>
  pivot_wider(names_from = class, values_from = HLA, names_prefix = "HLA_") |>
  rowwise() |>
  mutate(HLA_I = paste(HLA_I, collapse = ","),
         HLA_II = paste(HLA_II, collapse = ",")
         ) |>
  select(-flag)

HLApp_example_input_data<- left_join(y, x)

usethis::use_data(HLApp_example_input_data)

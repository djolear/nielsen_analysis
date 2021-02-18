data_path <- "D:/data/nielsen/regression_output"

## QFAHPD SPEND ##

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  ) %>% 
  filter(str_detect(file_list, "qh_spend"))

read_data_fn <- function(path) {
  df <-
    read_csv(paste0("D:/data/nielsen/regression_output/", path)) %>% 
    mutate(
      year = str_extract(path, "[[:digit:]]+")
    )
  
  return(df)
}

qh_spend_terms <-
  map_dfr(.x = file_list$file_list, .f = read_data_fn)

qh_spend_terms %>% 
  filter(term == "median_income_var_scale") %>% 
  ggplot(aes(year, estimate)) +
  geom_col() + 
  facet_grid(median_income_var ~ id_controls)

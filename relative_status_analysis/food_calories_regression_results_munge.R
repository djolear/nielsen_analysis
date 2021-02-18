data_path <- "D:/data/nielsen/regression_output"

## QFAHPD CALORIES RAW ##

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  ) %>% 
  filter(str_detect(file_list, "qh_calories_mi"))

read_data_fn <- function(path) {
  df <-
    read_csv(paste0("D:/data/nielsen/regression_output/", path)) %>% 
    mutate(
      year = str_extract(path, "[[:digit:]]+")
    )
  
  return(df)
}

qh_calories_terms <-
  map_dfr(.x = file_list$file_list, .f = read_data_fn)


## QFAHPD CALORIES SC ##

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  ) %>% 
  filter(str_detect(file_list, "qh_calories_sc_mi"))

read_data_fn <- function(path) {
  df <-
    read_csv(paste0("D:/data/nielsen/regression_output/", path)) %>% 
    mutate(
      year = str_extract(path, "[[:digit:]]+")
    )
  
  return(df)
}

qh_calories_sc_terms <-
  map_dfr(.x = file_list$file_list, .f = read_data_fn)


## QFAHPD CALORIES RAW IMPUTED ##

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  ) %>% 
  filter(str_detect(file_list, "qh_calories_imputed_mi"))

read_data_fn <- function(path) {
  df <-
    read_csv(paste0("D:/data/nielsen/regression_output/", path)) %>% 
    mutate(
      year = str_extract(path, "[[:digit:]]+")
    )
  
  return(df)
}

qh_calories_imputed_terms <-
  map_dfr(.x = file_list$file_list, .f = read_data_fn)


## QFAHPD CALORIES SC IMPUTED ##

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  ) %>% 
  filter(str_detect(file_list, "qh_calories_imputed_sc_mi"))

read_data_fn <- function(path) {
  df <-
    read_csv(paste0("D:/data/nielsen/regression_output/", path)) %>% 
    mutate(
      year = str_extract(path, "[[:digit:]]+")
    )
  
  return(df)
}

qh_calories_imputed_sc_terms <-
  map_dfr(.x = file_list$file_list, .f = read_data_fn)




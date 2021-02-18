create_new_upcs <- function(df){
  df <-
    df %>% 
    mutate(
      first_val = as.numeric(str_sub(upc, 12, 12)) * 3 + as.numeric(str_sub(upc, 10, 10)) * 3 + 
        as.numeric(str_sub(upc, 8, 8)) * 3 + as.numeric(str_sub(upc, 6, 6)) * 3 + 
        as.numeric(str_sub(upc, 4, 4)) * 3 + as.numeric(str_sub(upc, 2, 2)) * 3 +
        as.numeric(str_sub(upc, 11, 11)) + as.numeric(str_sub(upc, 9, 9)) +
        as.numeric(str_sub(upc, 7, 7)) + as.numeric(str_sub(upc, 5, 5)) +
        as.numeric(str_sub(upc, 3, 3)) + as.numeric(str_sub(upc, 1, 1)),
      
      mod_val = first_val %% 10,
      
      check_digit = ifelse(mod_val != 10, 10 - mod_val, 0),
      
      upc_new = paste0(upc, as.character(check_digit))
    )
  
  df <-
    df %>% 
    dplyr::select(-c(first_val, mod_val, check_digit))
  
  return(df)
}


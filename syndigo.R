df_prod_2020 <-
  read_csv("C:/Users/djole/Desktop/2020/Product.csv")

df_nutr <-
  read_csv("C:/Users/djole/Desktop/Nutrient.csv")


syndigo_products <-
  read_csv("C:/Users/djole/Desktop/2015/Product2015.csv")

syndigo_nutrients <-
  read_csv("C:/Users/djole/Desktop/2015/Nutrient.csv")

syndigo_nutrients_master <-
  read_csv("C:/Users/djole/Desktop/2015/NutrientMaster.csv")

syndigo_value_prepared <-
  read_csv("C:/Users/djole/Desktop/2015/ValuePrepared.csv")

products_master2 <-
  #products_master %>%  
  purchase %>% sample_n(500000) %>% 
  mutate(upc_new = paste0("0", upc)) %>% 
  left_join(
    df_prod_2020 %>% 
      mutate(upc_new = str_sub(UPC, 0, nchar(UPC) - 1)) %>% 
      dplyr::select(upc_new, HasNutrition_2020 = HasNutrition, Description)
  )

products_master2 %>% 
  count(HasNutrition_2020)


products_master <-
  products_master %>%  
  mutate(upc_new = paste0("0", upc))

products_master2 %>% 
  count(HasNutrition_2015)

products_master2 <-
  products_master2 %>% 
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
  ) %>% 
  left_join(
    li_upc
  )

products_master2 <-
  products_master2 %>% 
  left_join(products_master, by = c("upc", "upc_ver_uc"))

x <- products_master2 %>% 
  mutate(HasNutrition = coalesce(HasNutrition_2020, HasNutrition_2015)) %>% 
  count(HasNutrition == 1 | !is.na(calories_sc), department_descr) %>% 
  left_join(
    products_master2 %>% 
      count(department_descr) %>% 
      mutate(
        total = n
      ) %>% 
      dplyr::select(-n)
  ) %>% 
  mutate(per = n / total)



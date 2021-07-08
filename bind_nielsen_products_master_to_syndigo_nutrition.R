###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("broom.mixed", lib.loc = "/home/djolear/R")
library("lme4", lib.loc = "/home/djolear/R")



##############
##Functions ##
##############

'%!in%' <- function(x,y)!('%in%'(x,y))

bind_nutrition_info_to_products_master <- function(products_master, year) {
  
  syndigo_products_filename <-
    ifelse(
      year %in% c(2016:2020),
      paste0("/kilts/syndigo/", year, "/Product.csv"),
      paste0("/kilts/syndigo/", year, "/Product", year, ".csv")
    )
  
  syndigo_products <-
    read_csv(syndigo_products_filename)
  
  syndigo_nutrients <-
    read_csv(paste0("/kilts/syndigo/", year, "/Nutrient.csv"))
  
  syndigo_nutrients_master <-
    read_csv(paste0("/kilts/syndigo/", year, "/NutrientMaster.csv"))
  
  syndigo_value_prepared <-
    read_csv(paste0("/kilts/syndigo/", year, "/ValuePrepared.csv"))
  
  # syndigo_products <-
  #   syndigo_products %>% 
  #   mutate(
  #     upc_new = str_sub(UPC, 0, nchar(UPC) - 1)
  #   ) %>% 
  #   dplyr::select(
  #     upc_new,
  #     HasNutrition
  #   )
  # 
  # products_master <-
  #   products_master %>% 
  #   left_join(
  #     syndigo_products,
  #     by = "upc_new"
  #   )
  # 
  # products_master <-
  #   products_master %>% 
  #   filter(HasNutrition == 1)
  
  syndigo_nutrients_master <-
    syndigo_nutrients_master %>% 
    filter(Name == "Calories") 
  
  calories_master_id <-
    syndigo_nutrients_master$NutrientMasterID[1]
  
  syndigo_nutrients <- 
    syndigo_nutrients %>% 
    filter(
      (ValuePreparedType == 0 | is.na(ValuePreparedType)) & NutrientMasterID == 1
    ) %>% 
    mutate(
      calories_per_serving = abs(Quantity),
      upc_new = str_sub(UPC, 0, nchar(UPC) - 1)
    ) %>%  
    dplyr::select(
      upc_new,
      ValuePreparedType,
      calories_per_serving
    ) %>% 
    group_by(upc_new, ValuePreparedType) %>% 
    summarise(
      calories_per_serving = mean(calories_per_serving, na.rm = T)
    )
  
  duplicates <-
    syndigo_nutrients %>% 
    count(upc_new) %>% 
    filter(n > 1)
  
  syndigo_nutrients <-
    syndigo_nutrients %>% 
    filter(upc_new %!in% duplicates$upc_new)
  
  products_master <-
    products_master %>% 
    left_join(
      syndigo_nutrients,
      by = "upc_new"
    )
  
  syndigo_value_prepared <-
    syndigo_value_prepared %>% 
    filter(
      (ValuePreparedType == 0 | is.na(ValuePreparedType))
    ) %>% 
    mutate(
      upc_new = str_sub(UPC, 0, nchar(UPC) - 1)
    ) %>% 
    dplyr::select(
      upc_new,
      ValuePreparedType,
      servings = ServingsPerContainer
    )
  
  products_master <-
    products_master %>% 
    left_join(
      syndigo_value_prepared,
      by = c("upc_new", "ValuePreparedType")
    )
  
  products_master <-
    products_master %>% 
    mutate(
      servings = str_remove_all(servings, "approximately"),
      servings = str_remove_all(servings, "approx."),
      servings = str_remove_all(servings, "about"),
      servings = str_remove_all(servings, "approx"),
      servings = str_remove_all(servings, "usually"),
      servings = str_remove_all(servings, "servings")
    )
  
  products_master <-
    products_master %>% 
    mutate(
      servings = abs(as.numeric(servings)),
      calories_new = servings * as.numeric(calories_per_serving)
    )
  
  if(year == 2020) {
    products_master <-
      products_master %>% 
      mutate(
        calories_final = calories_new
      )
  } else {
    products_master <-
      products_master %>% 
      mutate(
        calories_final = coalesce(calories_final, calories_new)
      )
  }
  
  products_master <-
    products_master %>% 
    dplyr::select(
      upc:upc_new,
      calories_final
    )
  
  
  
  return(products_master)
  
}



products_master <- 
  readr::read_tsv("/kilts/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

products_master <-
  products_master %>%  
  mutate(upc_new = paste0("0", upc))

years <- c(seq(2020, 2012, -1), seq(2010, 2005, -1))

years 

for(i in 1:length(years)){
  products_master <- bind_nutrition_info_to_products_master(products_master, years[i])
  write_csv(products_master, "/project/ourminsk/nielsen/relative_status/syndigo/products_master_syndigo.csv")
  print(paste0( years[i], " complete"))
}


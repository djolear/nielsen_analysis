###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("broom.mixed", lib.loc = "/home/djolear/R")
library("lme4", lib.loc = "/home/djolear/R")



###########################
## Load Helper Functions ##
###########################

source("/home/djolear/nielsen/create_new_upcs_fn.R")

###################
## Main Function ##
###################

bind_products_master_syndigo_to_hms <- function(year) {
  
  purchase <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  products_master <- 
    readr::read_tsv("/project/ourminsk/nielsen/relative_status/syndigo/products_master_syndigo.csv")
  
  
  products <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))
  
  df <-
    purchase %>% 
    dplyr::select(
      -c(coupon_value, deal_flag_uc)
    ) %>% 
    left_join(
      trips %>% 
        dplyr::select(
          trip_code_uc,
          household_code,
          # total_spent,
          purchase_date 
        )
    ) %>% 
    mutate(
      month = month(purchase_date),
      quarter = quarter(purchase_date),
      year = year
    )
  
  df <-
    df %>%
    left_join(
      products_master %>%
        dplyr::select(
          product_module_descr,
          product_group_descr,
          department_descr,
          size1_amount,
          size1_units,
          upc,
          upc_ver_uc,
          calories_final
        )
    )
  
  gc()
  
  return(df)
  
}





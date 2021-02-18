# This script calculates milk-fat levels for milk products in the Nielsen Homescan data using the UPC description variable

# products_master = product master file
# products = product file for a given year


###################
## Load Packages ##
###################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)

###############
## Load Data ##
###############

products <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/2017/Annual_Files/trips_", year, ".tsv"))

products_master <- 
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv"))


###################
## Code Milk Fat ##
###################

milk_products <- 
  products_master %>% 
  filter(department_descr == "DAIRY" & product_group_descr == "MILK") %>% 
  mutate(
    # Search through the UPC description variable for the following strings
    milk_fat =
      case_when(
        stringr::str_detect(upc_descr, "SKM") == TRUE ~ 0,
        stringr::str_detect(upc_descr, "NF") == TRUE ~ 0,
        stringr::str_detect(upc_descr, "FF") == TRUE ~ 0,
        stringr::str_detect(upc_descr, "WH") == TRUE ~ 3.5,
        stringr::str_detect(upc_descr, "RF") == TRUE ~ 2, # IS THIS RIGHT? RF = REDUCED FAT?
        stringr::str_detect(upc_descr, "2%") == TRUE ~ 2,
        stringr::str_detect(upc_descr, "1%") == TRUE ~ 1,
        stringr::str_detect(upc_descr, "LF") == TRUE ~ 1,
        stringr::str_detect(upc_descr, "HALF/HALF") == TRUE ~ 12
      ),
    # Code specific dairy drink products
    egg_nog = ifelse(stringr::str_detect(upc_descr, "EGGNOG") == TRUE, 1, 0),
    bttr_milk = ifelse(stringr::str_detect(upc_descr, "BMLK") == TRUE, 1, 0),
    hvy_cream = ifelse(stringr::str_detect(upc_descr, "HEAVY") == TRUE, 1, 0),
    soy = ifelse(stringr::str_detect(upc_descr, "SOY") == TRUE, 1, 0),
    cream = ifelse(stringr::str_detect(upc_descr, "CR") == TRUE, 1, 0),
    shake = ifelse(stringr::str_detect(upc_descr, "SHK") == TRUE, 1, 0)
  ) %>% 
  mutate(
    # Classify milk as having low fat, no fat, or greater than/equal to regular fat
    low_fat_milk = ifelse(milk_fat < 2, 1, 0),
    non_fat_milk = ifelse(milk_fat == 0, 1, 0),
    reg_milk = 
      case_when(
        milk_fat > 1 ~ 1,
        egg_nog == 1 ~ 1,
        bttr_milk == 1 ~ 1,
        hvy_cream == 1 ~ 1,
        cream == 1 ~ 1,
        low_fat_milk == 1 ~ 0,
        non_fat_milk == 1 ~ 0,
        soy == 1 ~ -1
      )
  ) %>% 
  mutate(
    # Create a 13-digit UPC with a check digit
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

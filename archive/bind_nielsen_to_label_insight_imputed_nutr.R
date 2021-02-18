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


##############
## Set Path ##
##############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


########################
## Load Auxiliary Data ##
########################

# product_codes <-
#   read_csv("G:/My Drive/research/projects/niel/product_codes.csv")
# 
products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

# retailers <-
#   readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv")
# 
# product_modules <- read_csv(paste0(machine_path, "research/projects/data1/nielsen_product_modules.csv"))
# 
# products_master <-
#   products_master %>% 
#   left_join(
#     product_modules %>% 
#       dplyr::select(-n)
#   )

nf_impute <-
  read_csv(paste0(machine_path, "research/projects/niel/nielsen_data_output/products_master_nf_impute.csv"))



bind_nielsen_to_imputed_nutrition_info <- function(year, nf_impute) {
  
  purchase <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
  panelists <- 
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  products <- 
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))
  
  niel_df <-
    purchase %>% 
    left_join(
      trips %>% 
        dplyr::select(
          trip_code_uc,
          household_code,
          total_spent,
          purchase_date 
        )
    )
  
  trips <-
    trips %>% 
    mutate(
      month = lubridate::month(purchase_date),
      quarter = lubridate::quarter(purchase_date)
    ) 
  
  
  # # Currently looking only at food stores
  # trips <- 
  #   trips %>% 
  #   left_join(
  #     retailers %>% 
  #       filter(
  #         channel_type %in% 
  #           c(
  #             "Grocery", 
  #             "Bakery", 
  #             "Beverage Store", 
  #             "Bodega", "Butcher", 
  #             "Candy Store", 
  #             "Cheese Stores", 
  #             "Coffee Store/Gourmet Coffee",
  #             "Convenience Store",
  #             "Dairy Store",
  #             "Delicatessen",
  #             "Dollar Store",
  #             "Drug Store",
  #             "Fish Market",
  #             "Fruit Stand",
  #             "as Mini Mart",
  #             "Health Food Store",
  #             "Pizzeria",
  #             "Quick Serve Restaurants",
  #             "Restaurant",
  #             "Vending Machine"
  #           )
  #       )
  #   )
  
  niel_df <-
    niel_df %>% 
    left_join(
      nf_impute,
      by = c("upc", "upc_ver_uc")
    )
  
  niel_df <-
    niel_df %>% 
    left_join(
      products_master %>% 
        dplyr::select(
          upc,
          upc_ver_uc,
          product_module_descr,
          product_group_descr,
          department_descr,
          size1_amount,
          size1_units
        ),
      by = c("upc", "upc_ver_uc")
    )
  

  rm(purchase, trips, panelists, products, niel_df)
  
  return(niel_df)
  
}




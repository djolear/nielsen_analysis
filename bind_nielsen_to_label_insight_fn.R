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
# products_master <-
#   readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")
# 
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


###########################
## Load Helper Functions ##
###########################

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/load_and_convert_label_insight_upcs.R"))

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/create_new_upcs_fn.R"))

###################
## Main Function ##
###################

bind_labels_to_purchases_fn <- function(year) {
  
  purchase <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
  # panelists <-
  #   readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  # 
  # products <-
  #   readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))

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
  

  niel_df <- create_new_upcs(niel_df)
  
  niel_df <-
    niel_df %>%
    left_join(
      li_upc %>%
        dplyr::select(
          upc_new,
          sugar = Sugars,
          calories = Calories,
          saturated_fat = `Saturated Fat`
        ),
      by = "upc_new"
    )

  
  rm(purchase, trips)
  
  return(niel_df)
  
}





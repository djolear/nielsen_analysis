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

products_master <- 
  readr::read_tsv("/kilts/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

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
  
  
  
}
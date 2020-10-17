if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"),
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


## Load Data ##



bind_zip_code_census_data_function <- function(nps) {

  ########################
  ## Zipcode City Names ##
  ########################
  
  source(paste0(machine_path, "research/projects/data1/geography/zipcodes_city_names.R"))
  
  nps <-
    nps %>% 
    left_join(
      zipcodes %>% mutate(zip = as.character(zip))
    )  
  
  ################
  ## Census SES ##
  ################
  
  census_ses_zip_wide <- 
    read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_", year, ".csv"))
  
  nps <-
    nps %>% 
    left_join(
      census_ses_zip_wide,
      by = "zip"
    )
  
  nps <-
    nps %>% 
    mutate(
      inc_diff = inc_mid - median_income
    )
  
  
  ##############
  ## Mobility ##
  ##############
  
  source(paste0(machine_path, "research/projects/data1/mobility/mobility_county2zip.R"))
  
  nps <-
    nps %>% 
    left_join(
      zip_mob,
      by = "zip"
    )
  
  return(nps)
}















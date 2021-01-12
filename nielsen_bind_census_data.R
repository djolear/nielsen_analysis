
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


#########
## ... ##
#########


source(paste0(machine_path, "research/projects/niel/nielsen_analysis/calculate_nutrition_per_cal_fn.R"))

year = "2017"

npc_2017 <- 
  nutrition_per_cal_fn(year)

npc_2017 <-
  bind_zip_code_census_data_function(npc_2017, year)  


year = "2016"

npc_2016 <- 
  nutrition_per_cal_fn(year)

npc_2016 <-
  bind_zip_code_census_data_function(npc_2016, year)  


year = "2015"

npc_2015 <- 
  nutrition_per_cal_fn(year)

write_rds(npc_2015, paste0(machine_path, "research/projects/niel/nielsen_data_output/npc_", year, ".rds"))

npc_2015 <-
  bind_zip_code_census_data_function(npc_2015, year)  


year = "2014"

npc_2014 <- 
  nutrition_per_cal_fn(year)

write_rds(npc_2014, paste0(machine_path, "research/projects/niel/nielsen_data_output/npc_", year, ".rds"))


npc_2014 <-
  bind_zip_code_census_data_function(npc_2014, year)  


year = "2013"

npc_2013 <- 
  nutrition_per_cal_fn(year)

write_rds(npc_2013, paste0(machine_path, "research/projects/niel/nielsen_data_output/npc_", year, ".rds"))


npc_2013 <-
  bind_zip_code_census_data_function(npc_2013, year)  


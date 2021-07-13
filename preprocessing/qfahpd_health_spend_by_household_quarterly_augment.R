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


###################
## Load Packages ##
###################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven
)


########################
## Load Main Function ##
########################

source("G:/My Drive/research/projects/niel/nielsen_relative_income/preprocessing/qfahpd_health_by_household_augment.R")


######################
## Augment the Data ##
######################

# Set data path
data_path <- "D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_quarterly/"


# Get list of file names
file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path, "nielsen_only/"))
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv")
  ) %>% 
  slice(1:16)


# Load reference group incomes
reference_group_incomes <-
  read_csv("D:/data/nielsen/ml/rf_income_predictions_default_070721.csv")


# Loop over years and munge data
for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("qh_spend_by_household_quarterly_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    add_secondary_and_save(
      data_path = data_path,
      file_name = file_list$file_list[i], 
      current_year = as.numeric(str_extract(file_list$file_list[i], "[[:digit:]]+")),
      reference_group_incomes = reference_group_incomes,
      file_name_stem = "qh_spend_by_household_quarterly_wide_secondary_"
    )
  )
  print(paste0(str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}


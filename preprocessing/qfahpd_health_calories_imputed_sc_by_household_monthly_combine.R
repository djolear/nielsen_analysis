

###################
## Load Packages ##
###################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven
)


###############
## Set Paths ##
###############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

data_path <- "research/projects/gallup/gallup_data/relative_status/"

###############
## Load Data ##
###############

file_list <- 
  data.frame(
    file_list = list.files(path = "D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/with_secondary_data")
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv")
  )  

for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("qh_calories_imputed_sc_by_household_monthly_wide_secondary_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    read_csv(paste0("D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/with_secondary_data/", file_list$file_list[i]))
  )
  print(paste0("load ", str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}

qh_calories_imputed_sc_by_household_monthly <-
  bind_rows(
    qh_calories_imputed_sc_by_household_monthly_2004,
    qh_calories_imputed_sc_by_household_monthly_2005,
    qh_calories_imputed_sc_by_household_monthly_2006,
    qh_calories_imputed_sc_by_household_monthly_2007,
    qh_calories_imputed_sc_by_household_monthly_2008,
    qh_calories_imputed_sc_by_household_monthly_2009,
    qh_calories_imputed_sc_by_household_monthly_2010,
    qh_calories_imputed_sc_by_household_monthly_2011,
    qh_calories_imputed_sc_by_household_monthly_2012,
    qh_calories_imputed_sc_by_household_monthly_2013,
    qh_calories_imputed_sc_by_household_monthly_2014,
    qh_calories_imputed_sc_by_household_monthly_2015,
    qh_calories_imputed_sc_by_household_monthly_2016,
    qh_calories_imputed_sc_by_household_monthly_2017,
    qh_calories_imputed_sc_by_household_monthly_2018,
    qh_calories_imputed_sc_by_household_monthly_2019
  )

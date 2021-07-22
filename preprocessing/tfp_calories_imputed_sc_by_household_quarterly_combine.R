

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

###############
## Load Data ##
###############

file_list <- 
  data.frame(
    file_list = list.files(path = "D:/data/nielsen/calories_extracts/tfp_calories_imputed_sc_by_household_quarterly/with_secondary_data")
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, "tp_") & file_list != "qh_calories_imputed_sc_by_household_quarterly_wide_secondary_2p_2008.csv"
  )  

for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("tfp_isc_qr_sec_tp_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    read_csv(paste0("D:/data/nielsen/calories_extracts/tfp_calories_imputed_sc_by_household_quarterly/with_secondary_data/", file_list$file_list[i]))
  )
  print(paste0("load ", str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}

tfp_isc_qr_sec_tp <-
  bind_rows(
    tfp_isc_qr_sec_tp_2004,
    tfp_isc_qr_sec_tp_2005,
    tfp_isc_qr_sec_tp_2006,
    tfp_isc_qr_sec_tp_2007,
    tfp_isc_qr_sec_tp_2008,
    tfp_isc_qr_sec_tp_2009,
    tfp_isc_qr_sec_tp_2010,
    tfp_isc_qr_sec_tp_2011,
    tfp_isc_qr_sec_tp_2012,
    tfp_isc_qr_sec_tp_2013,
    tfp_isc_qr_sec_tp_2014,
    tfp_isc_qr_sec_tp_2015,
    tfp_isc_qr_sec_tp_2016,
    tfp_isc_qr_sec_tp_2017,
    tfp_isc_qr_sec_tp_2018,
    tfp_isc_qr_sec_tp_2019
  )


write_csv(tfp_isc_qr_sec_tp , "D:/data/nielsen/calories_extracts/tfp_calories_imputed_sc_by_household_quarterly/combined/tfp_isc_qr_sec_tp.csv")

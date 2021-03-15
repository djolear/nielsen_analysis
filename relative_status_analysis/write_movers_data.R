library(tidyverse)

################
## FOOD SPEND ##
################

zip_movers_1yr <- function(year){
  
  year1 = year
  year2 = year - 1
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(zip_t1 != zip_t2) 
  
  return(movers)  
}

years <- seq(2005, 2019, 1)

zip_movers_1yr_spend_data <-
  map_dfr(.x = years, .f = zip_movers_1yr)

write_csv(zip_movers_1yr_spend_data, "D:/data/nielsen/mover_data/zip_movers_1yr_spend_data.csv")


zip_movers_2yr <- function(year){
  
  year1 = year
  year2 = year - 2
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(zip_t1 != zip_t2) 
  
  return(movers)  
}

years <- seq(2006, 2019, 1)

zip_movers_2yr_spend_data <-
  map_dfr(.x = years, .f = zip_movers_2yr)

write_csv(zip_movers_2yr_spend_data, "D:/data/nielsen/mover_data/zip_movers_2yr_spend_data.csv")


fips_movers_1yr <- function(year){
  
  year1 = year
  year2 = year - 1
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(fips_code_t1 != fips_code_t2) 
  
  return(movers)  
}

years <- seq(2005, 2019, 1)

fips_movers_1yr_spend_data <-
  map_dfr(.x = years, .f = fips_movers_1yr)

write_csv(fips_movers_1yr_spend_data, "D:/data/nielsen/mover_data/fips_movers_1yr_spend_data.csv")


fips_movers_2yr <- function(year){
  
  year1 = year
  year2 = year - 2
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_spend_by_household_quarterly/qh_spend_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(fips_code_t1 != fips_code_t2) 
  
  return(movers)  
}

years <- seq(2006, 2019, 1)

fips_movers_2yr_spend_data <-
  map_dfr(.x = years, .f = fips_movers_2yr)

write_csv(fips_movers_2yr_spend_data, "D:/data/nielsen/mover_data/fips_movers_2yr_spend_data.csv")


#########################
## CALORIE CONSUMPTION ##
#########################

zip_movers_1yr <- function(year){
  
  year1 = year
  year2 = year - 1
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(zip_t1 != zip_t2) 
  
  return(movers)  
}

years <- seq(2005, 2019, 1)

zip_movers_1yr_calorie_data <-
  map_dfr(.x = years, .f = zip_movers_1yr)

write_csv(zip_movers_1yr_calorie_data, "D:/data/nielsen/mover_data/zip_movers_1yr_calorie_data.csv")


zip_movers_2yr <- function(year){
  
  year1 = year
  year2 = year - 2
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(zip_t1 != zip_t2) 
  
  return(movers)  
}

years <- seq(2006, 2019, 1)

zip_movers_2yr_calorie_data <-
  map_dfr(.x = years, .f = zip_movers_2yr)

write_csv(zip_movers_2yr_calorie_data, "D:/data/nielsen/mover_data/zip_movers_2yr_calorie_data.csv")


fips_movers_1yr <- function(year){
  
  year1 = year
  year2 = year - 1
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(fips_code_t1 != fips_code_t2) 
  
  return(movers)  
}

years <- seq(2005, 2019, 1)

fips_movers_1yr_calorie_data <-
  map_dfr(.x = years, .f = fips_movers_1yr)

write_csv(fips_movers_1yr_calorie_data, "D:/data/nielsen/mover_data/fips_movers_1yr_calorie_data.csv")


fips_movers_2yr <- function(year){
  
  year1 = year
  year2 = year - 2
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year2, ".csv"))
  
  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(fips_code_t1 != fips_code_t2) 
  
  return(movers)  
}

years <- seq(2006, 2019, 1)

fips_movers_2yr_calorie_data <-
  map_dfr(.x = years, .f = fips_movers_2yr)

write_csv(fips_movers_2yr_calorie_data, "D:/data/nielsen/mover_data/fips_movers_2yr_calorie_data.csv")

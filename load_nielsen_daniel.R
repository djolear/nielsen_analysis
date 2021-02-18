year = 2014

purchase <-
  readr::read_tsv(
    paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"), n_max = 5000000
  )

trips <-
  readr::read_tsv(
    paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"),
    n_max = 10000
  )

panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

products_master <- 
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv"))


products <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))

niel_df <-
  purchase %>% 
  dplyr::select(
    -c(coupon_value, deal_flag_uc)
  ) %>% 
  left_join(
    trips %>% 
      dplyr::select(
        trip_code_uc,
        household_code,
        # total_spent,
        purchase_date 
      )
  )


df <- read_csv("G:/My Drive/research/projects/niel/nielsen_data_output/group_calories_by_household_monthly_wide_2005.csv")

li_upc <- read_csv("G:/My Drive/research/projects/niel/li_upc_update_011021.csv")

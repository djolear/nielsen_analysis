year = 2014

purchase <-
  readr::read_tsv(
    paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv")
  )

trips <-
  readr::read_tsv(
    paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv")
  )

panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

products_master <- 
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv"))


products_master <-
  products_master %>% 
  mutate(
    first_val = as.numeric(str_sub(upc, 12, 12)) * 3 + as.numeric(str_sub(upc, 10, 10)) * 3 + 
      as.numeric(str_sub(upc, 8, 8)) * 3 + as.numeric(str_sub(upc, 6, 6)) * 3 + 
      as.numeric(str_sub(upc, 4, 4)) * 3 + as.numeric(str_sub(upc, 2, 2)) * 3 +
      as.numeric(str_sub(upc, 11, 11)) + as.numeric(str_sub(upc, 9, 9)) +
      as.numeric(str_sub(upc, 7, 7)) + as.numeric(str_sub(upc, 5, 5)) +
      as.numeric(str_sub(upc, 3, 3)) + as.numeric(str_sub(upc, 1, 1)),
    
    mod_val = first_val %% 10,
    
    check_digit = ifelse(mod_val != 10, 10 - mod_val, 0),
    
    upc_new = paste0(upc, as.character(check_digit))
  ) %>% 
  dplyr::select(-c(first_val, mod_val, check_digit))


products <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"), n_max = 1000)

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

niel_df <- 
  niel_df %>% 
  left_join(
    products_master
  )




# df <- read_csv("G:/My Drive/research/projects/niel/nielsen_data_output/group_calories_by_household_monthly_wide_2005.csv")
# 
li_upc <- read_csv("G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_sc_cal_only_021721.csv")

li_upc <-
  li_upc %>% 
  mutate(
    char = nchar(UPC)
  )

li_upc <-
  li_upc %>% 
  mutate(
    upc_new = ifelse(char == 12, paste0("0", UPC), UPC)
  )

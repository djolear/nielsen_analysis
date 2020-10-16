library(tidyverse)

purchase17 <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/2017/Annual_Files/purchases_2017.tsv")

trips17 <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/2017/Annual_Files/trips_2017.tsv")

panelists17 <- 
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/2017/Annual_Files/panelists_2017.tsv")

products17 <- 
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/2017/Annual_Files/products_extra_2017.tsv")

products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

product_codes <-
  read_csv("G:/My Drive/research/projects/niel/product_codes.csv")

purchase17_sub <-
  purchase17 %>% 
  slice(1:664542)

purchase17_sub <-
  purchase17 %>% 
  slice(1:1000)

niel17_df <-
  purchase17 %>% 
  left_join(
    trips17 %>% 
      dplyr::select(
        trip_code_uc,
        household_code,
        total_spent,
        purchase_date 
      )
  )

trips17 <-
  trips17 %>% 
  mutate(
    month = lubridate::month(purchase_date),
    quarter = lubridate::quarter(purchase_date)
  ) 

spend17 <-
  trips17 %>% 
  group_by(household_code) %>% 
  summarise(
    total_spend = sum(total_spent, na.rm = TRUE)
  )

# niel17_df <-
#   niel17_df %>% 
#   left_join(
#     panelists17 %>% mutate(household_code = Household_Cd)
#   )

# purchase17_sub <-
#   purchase17_sub %>% 
#   left_join(
#     products17,
#     by = c("upc", "upc_ver_uc")
#   )

product_modules <- read_csv(paste0(machine_path, "research/projects/data1/nielsen_product_modules.csv"))

products_master <-
  products_master %>% 
  left_join(
    product_modules %>% 
    dplyr::select(-n)
  )

niel17_df <-
  niel17_df %>% 
  left_join(
    products_master %>% 
      dplyr::select(
        product_group_descr,
        class,
        upc,
        upc_ver_uc
      ),
    by = c("upc", "upc_ver_uc")
  )

spend_per_cat17 <-
  niel17_df %>% 
  group_by(
    household_code,
    class
  ) %>% 
  summarise(
    spend_per_cat = sum(total_price_paid)
  )

spend_per_cat17 <-
  spend_per_cat17 %>% 
  left_join(
    spend17
  )

spend_per_cat17 <-
  spend_per_cat17 %>% 
  mutate(
    spend_per_cat_per = spend_per_cat / total_spend
  )

spend_per_cat17 <-
  spend_per_cat17 %>% 
  left_join(
    panelists17 %>% mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income,
        Household_Size,
        Male_Head_Education,
        Female_Head_Education,
        Panelist_ZipCd,
        Fips_State_Cd,
        Fips_County_Cd,
        Panelist_ZipCd
      )
  )

spend_per_cat17 <-
  spend_per_cat17 %>% 
  mutate(
    spend_per_cat_inc = spend_per_cat / Household_Income
  )

spend_per_cat17 <-
  spend_per_cat17 %>% 
  ungroup %>% 
  mutate(
    state_fips = ifelse(str_length(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
    county_fips = ifelse(str_length(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd),
    fips_code = paste0(state_fips, county_fips)
  )

# candy <- spend_per_cat17 %>% filter(product_group_descr == "CANDY")
# 
# product_modules <-
#   products_master %>% 
#   filter(
#     department_descr %in% 
#       c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT")
#   ) %>% 
#   count(product_module_descr)

#write_csv(product_modules, paste0(machine_path, "research/projects/data1/nielsen_product_modules.csv"))

product_modules <- read_csv(paste0(machine_path, "research/projects/data1/nielsen_product_modules.csv"))

ocounty_candy  <-
  candy %>% 
  group_by(fips_code) %>% 
  summarise(
    avg_candy = mean(spend_per_cat_inc)
  )

zipcode_candy  <-
  candy %>% 
  group_by(Panelist_ZipCd) %>% 
  summarise(
    avg_candy = mean(spend_per_cat_inc)
  )

zipcode_candy <-
  zipcode_candy %>% 
  mutate(zip  = Panelist_ZipCd ) %>% 
  left_join(
    census_ses_zipcode_wide_2017,
    by = "zip"
  )

census_ses_zipcode_wide_2017 <- read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_2017.csv"))

candy <-
  candy %>% 
  mutate(zip  = Panelist_ZipCd ) %>% 
  left_join(
    census_ses_zipcode_wide_2017,
    by = "zip"
  )
##

niel17_df <-
  niel17_df %>% 
  left_join(
    product_codes,
    by = "product_module_code"
  )

purchase17_sub %>% count(is.na(product_module_descr))

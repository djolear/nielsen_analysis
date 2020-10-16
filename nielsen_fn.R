library(tidyverse)

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


product_codes <-
  read_csv("G:/My Drive/research/projects/niel/product_codes.csv")

products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

retailers <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv")


product_modules <- read_csv(paste0(machine_path, "research/projects/data1/nielsen_product_modules.csv"))

products_master <-
  products_master %>% 
  left_join(
    product_modules %>% 
      dplyr::select(-n)
  )


nf <- function(year) {
  
  purchase <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
  panelists <- 
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  products <- 
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))
  
  niel_df <-
    purchase %>% 
    left_join(
      trips %>% 
        dplyr::select(
          trip_code_uc,
          household_code,
          total_spent,
          purchase_date 
        )
    )
  
  trips <-
    trips %>% 
    mutate(
      month = lubridate::month(purchase_date),
      quarter = lubridate::quarter(purchase_date)
    ) 
  
  trips <- 
    trips %>% 
    left_join(
      retailers %>% 
        filter(
          channel_type %in% 
            c(
              "Grocery", 
              "Bakery", 
              "Beverage Store", 
              "Bodega", "Butcher", 
              "Candy Store", 
              "Cheese Stores", 
              "Coffee Store/Gourmet Coffee",
              "Convenience Store",
              "Dairy Store",
              "Delicatessen",
              "Dollar Store",
              "Drug Store",
              "Fish Market",
              "Fruit Stand",
              "as Mini Mart",
              "Health Food Store",
              "Pizzeria",
              "Quick Serve Restaurants",
              "Restaurant",
              "Vending Machine"
            )
        )
    )
  
  spend <-
    trips %>% 
    #filter(!is.na(channel_type)) %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_spent, na.rm = TRUE)
    )
  
  
  
  niel_df <-
    niel_df %>% 
    left_join(
      products_master %>% 
        dplyr::select(
          product_group_descr,
          product_module_descr,
          department_descr,
          class,
          upc,
          upc_ver_uc,
          upc_new
        ),
      by = c("upc", "upc_ver_uc")
    )
  
  ###
  
  niel_df <-
    niel_df %>% 
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
    )
  
  niel_df <-
    niel_df %>%
    left_join(
      li_upc %>%
        dplyr::select(
          upc_new,
          sugar = Sugars,
          cal = Calories
        ),
      by = "upc_new"
    )
  # 
  # sug <-
  #   niel_df %>% 
  #   group_by(household_code) %>% 
  #   summarise(sug = sum(sug, na.rm = TRUE))
  
  spend_per_cat <-
    niel_df %>%
    group_by(
      household_code,
      product_module_descr
    ) %>%
    summarise(
      spend_per_cat = sum(total_price_paid)
    )
  
  spend_per_cat <-
    spend_per_cat %>% 
    left_join(
      spend,
      by = "household_code"
    )
  
  spend_per_cat <-
    spend_per_cat %>% 
    mutate(
      spend_per_cat_per = spend_per_cat / total_spend
    )
  
  # sug <- 
  #   sug %>% 
  #   left_join(
  #     panelists %>% mutate(household_code = Household_Cd) %>% 
  #       dplyr::select(
  #         household_code,
  #         Household_Income,
  #         Household_Size,
  #         Male_Head_Education,
  #         Female_Head_Education,
  #         Panelist_ZipCd,
  #         Fips_State_Cd,
  #         Fips_County_Cd,
  #         Panelist_ZipCd
  #       )
  #   )
  
  spend_per_cat <-
    spend_per_cat %>% 
    left_join(
      panelists %>% mutate(household_code = Household_Cd) %>% 
        dplyr::select(
          household_code,
          Household_Income,
          Household_Size,
          Male_Head_Education,
          Female_Head_Education,
          Panelist_ZipCd,
          Fips_State_Cd,
          Fips_County_Cd,
          Panelist_ZipCd,
          inc_mid
        )
    )
  
  spend_per_cat <-
    spend_per_cat %>% 
    mutate(
      spend_per_cat_inc = spend_per_cat / Household_Income
    )
  
  spend_per_cat <-
    spend_per_cat %>% 
    ungroup %>% 
    mutate(
      state_fips = ifelse(str_length(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      county_fips = ifelse(str_length(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd),
      fips_code = paste0(state_fips, county_fips)
    )
  
  #rm(purchase, trips, panelists, products, niel_df)
  
  return(spend_per_cat)
  
}


#years = list("2012", "2013", "2014", "2015")

spend_per_cat12 <- nf("2012")
spend_per_cat13 <- nf("2013")
spend_per_cat14 <- nf("2014")
spend_per_cat15 <- nf("2015")

summary(lm(spend_per_cat_per ~ log(Household_Income + 1), data = spend_per_cat %>%  filter(product_module_descr == "CANDY-NON-CHOCOLATE")))


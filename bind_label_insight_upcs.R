
sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


li_upc <-
  read_csv(paste0(machine_path, "research/projects/niel/label_insight_upc.csv"))


# niel_df_sub <-
#   niel_df %>% 
#   slice(1:60000)



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
  )

products <-
  products %>% 
  mutate(
    first_val = as.numeric(str_sub(upc, 12, 12)) * 3 + as.numeric(str_sub(upc, 10, 10)) * 3 + 
      as.numeric(str_sub(upc, 8, 8)) * 3 + as.numeric(str_sub(upc, 6, 6)) * 3 + 
      as.numeric(str_sub(upc, 4, 4)) * 3 + as.numeric(str_sub(upc, 2, 2)) * 3 +
      as.numeric(str_sub(upc, 11, 11)) + as.numeric(str_sub(upc, 9, 9)) +
      as.numeric(str_sub(upc, 7, 7)) + as.numeric(str_sub(upc, 5, 5)) +
      as.numeric(str_sub(upc, 3, 3)) + as.numeric(str_sub(upc, 1, 1)),
    
    mod_val = first_val %% 10,
    
    check_digit = ifelse(mod_val != 0, 10 - mod_val, 0),
    
    upc_new = paste0(upc, as.character(check_digit)),
    upc_new2 = str_sub(upc, 2, 13)
  )

products2 <-
  products %>% 
  left_join(
    li_upc %>% 
      dplyr::select(
        upc_new,
        Brand,
        sugar = Sugars,
        cal = Calories
      ),
    by = "upc_new"
  )

products2 <-
  products2 %>% 
  left_join(
    products_master,
    by = c("upc", "upc_ver_uc")
  )

.products_master2 <-
  products_master %>% 
  left_join(
    li_upc %>% 
      dplyr::select(
        upc_new,
        sugar = Sugars,
        cal = Calories
      ),
    by = "upc_new"
  )

products2 %>% 
  count(department_descr,is.na(cal)) %>% 
  left_join(
    products_master %>% 
      count(department_descr) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>% 
  mutate(
    per = n/total
  ) %>% 
  filter(`is.na(cal)` == FALSE) %>% 
  ggplot(aes(department_descr, per)) + 
  geom_col() + 
  coord_flip()


niel_df %>% 
  count(department_descr, !is.na(sugar)) %>% 
  left_join(
    niel_df %>% 
      count(department_descr) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  mutate(per = n / total) %>% 
  filter(`!is.na(sugar)` == TRUE) 


niel_df %>% 
  filter(department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "NON-FOOD GROCERY", "PACKAGED MEAT", NA)) %>% 
  count(.)

niel_df %>% 
  filter(department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "NON-FOOD GROCERY", "PACKAGED MEAT", NA)) %>% 
  count(!is.na(sugar)) %>% 
  mutate(per = n / 58771129) %>% 
  filter(`!is.na(sugar)` == TRUE) 


household_sugar_label_na <-
  niel_df %>% 
  count(household_code, !is.na(sugar)) %>% 
  left_join(
    niel_df %>% 
      count(household_code) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  mutate(per = n / total) %>% 
  filter(`!is.na(sugar)` == TRUE) 

household_sugar_label_na <-
  household_sugar_label_na %>% 
  left_join(
    panelists %>% mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income
      )
  )

cor.test(household_sugar_label_na$Household_Income, household_sugar_label_na$per)

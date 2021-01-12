products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

fresh_produce <-
  products_master %>% 
  filter(product_group_descr == "FRESH PRODUCE")

fresh_produce <-
  fresh_produce %>% 
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

fresh_produce <-
  fresh_produce %>% 
  dplyr::select(-c(first_val, mod_val, check_digit))

fresh_produce <-
  fresh_produce %>% 
  left_join(
    li_upc %>%
      dplyr::select(
        upc_new,
        sugar = Sugars,
        add_sugars = `Added Sugars`,
        calories = Calories,
        saturated_fat = `Saturated Fat`
      ),
    by = "upc_new"
  )

# Convert pounds to ounces
fresh_produce <-
  fresh_produce %>% 
  mutate(
    size_new =
      case_when(
        size1_units == "PO" ~ size1_amount * 16,
        TRUE ~ size1_amount
      ),
    units_new =
      case_when(
        size1_units == "PO" ~ "OZ",
        TRUE ~ size1_units
      )
  )

# Calculate calories per unit for the products that we have calories for
calories_per_unit <- 
  fresh_produce %>% 
  filter(
    product_group_descr == "FRESH PRODUCE" & !is.na(calories)
  ) %>% 
  mutate(
    calories_per_unit = calories / size_new 
  ) %>% 
  group_by(product_module_descr, units_new) %>% 
  summarise(
    mean_cal = mean(calories_per_unit, na.rm = TRUE),
    median_cal = median(calories_per_unit, na.rm = TRUE),
    sd = sd(calories_per_unit, na.rm = TRUE)
  ) 


# Add in data about how many products in each cell we have data for
calories_per_unit <- 
  calories_per_unit %>% 
  left_join(
    fresh_produce %>% 
      filter(
        product_group_descr == "FRESH PRODUCE" & !is.na(calories)
      ) %>% 
      count(product_module_descr, units_new)
  )

# Filter out cells that have less than 2 products
calories_per_unit <-
  calories_per_unit %>% 
  filter(n > 1)

fresh_produce <-
  fresh_produce %>% 
  left_join(
    calories_per_unit %>% 
      dplyr::select(
        product_module_descr,
        units_new,
        median_cal
      ) %>% 
      ungroup()
  )

fresh_produce <-
  fresh_produce %>% 
  mutate(
    calories = 
      case_when(
        !is.na(calories) == TRUE ~ calories,
        is.na(calories) == TRUE ~ median_cal * size_new
      )
  ) %>% 
  count(product_group_descr == "FRESH PRODUCE", is.na(calories))



products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

'%!in%' <- function(x,y)!('%in%'(x,y))

products_master <-
  products_master %>% 
  filter(department_descr %!in% c("HEALTH & BEAUTY CARE", "NON-FOOD GROCERY", "GENERAL MERCHANDISE", "MAGNET DATA")) %>% 
  filter(size1_units %in% c("CT", "OZ", "PO"))

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

products_master <-
  products_master %>% 
  dplyr::select(-c(first_val, mod_val, check_digit))

li_upc <-
  read_csv("G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_raw_cal_only_021721.csv")

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
  left_join(
    li_upc %>%
      dplyr::select(
        upc_new,
        calories = Calories
        # sugar = Sugars,
        # add_sugars = `Added Sugars`,
        # saturated_fat = `Saturated Fat`
      ),
    by = "upc_new"
  )

# Convert pounds to ounces
products_master <-
  products_master %>% 
  mutate(
    size_new =
      case_when(
        size1_units == "PO" ~ size1_amount * 16,
        size1_units == "LI" ~ size1_amount * 16 * 2.2,
        size1_units == "ML" ~ (size1_amount*16*2.2)/1000,
        size1_units == "QT" ~ size1_amount*16*2.08,
        TRUE ~ size1_amount
      ),
    units_new =
      case_when(
        size1_units == "PO" ~ "OZ",
        size1_units == "LI" ~ "OZ",
        size1_units == "ML" ~ "OZ",
        size1_units == "QT" ~ "OZ",
        TRUE ~ size1_units
      )
  )

# Calculate calories per unit for the products that we have calories for
calories_per_unit <- 
  products_master %>% 
  filter(
   !is.na(calories)
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
    products_master %>% 
      filter(
        !is.na(calories)
      ) %>% 
      count(product_module_descr, units_new)
  )

# Filter out cells that have less than 2 products
calories_per_unit <-
  calories_per_unit %>% 
  filter(n > 5)

products_master <-
  products_master %>% 
  left_join(
    calories_per_unit %>% 
      dplyr::select(
        product_module_descr,
        units_new,
        median_cal
      ) %>% 
      ungroup()
  )

products_master <-
  products_master %>% 
  mutate(
    calories_imp = 
      case_when(
        !is.na(calories) == TRUE ~ calories,
        is.na(calories) == TRUE ~ median_cal * size_new
      )
  ) 

products_master %>% count(is.na(calories))
products_master %>% count(is.na(calories_imp))

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

data_path <- "research/projects/niel/nielsen_data_output/imputed_data/"

write_csv(products_master, paste0(machine_path, data_path, "products_master_imputed_calories.csv"))



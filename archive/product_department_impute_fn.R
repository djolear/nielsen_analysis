
###################
## Load Packages ##
###################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)


##############
## Set Path ##
##############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


#########################
## Load Auxiliary Data ##
#########################

products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/load_and_convert_label_insight_upcs.R"))


department_nutrition_impute_fn <- function(products_master, x_nutrient, department, li_upc_update) {
  df <-
    products_master %>% 
    filter(department_descr == department)
  
  df <-
    df %>% 
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
  
  df <-
    df %>% 
    dplyr::select(-c(first_val, mod_val, check_digit))
  
  df <-
    df %>% 
    left_join(
      li_upc_update %>%
        dplyr::select(
          upc_new,
          sugar,
          add_sugars,
          calories,
          saturated_fat,
          sodium
        ),
      by = "upc_new"
    )
  
  # Add column for nutrient of interest based on function argument
  df <-
    df %>% 
    dplyr::select(
      upc:sodium,
      nutrient = !!enquo(x_nutrient)
    )
  
  # Convert pounds to ounces
  df <-
    df %>% 
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
  
  # Calculate nutrient per unit for the products that we have nutrients for
  #nutrient <- enquos(nutrient)
  
  nutrient_per_unit <- 
    df %>% 
    dplyr::select(
      department_descr,
      product_group_descr,
      product_module_descr,
      units_new, 
      nutrient,
      size_new
    ) %>% 
    filter(
      department_descr == department & !is.na(nutrient)
    ) %>% 
    mutate(
      nutrient_per_unit = nutrient / size_new 
    ) %>% 
    group_by(product_module_descr, units_new) %>% 
    summarise(
      mean_nutr = mean(nutrient_per_unit, na.rm = TRUE),
      median_nutr = median(nutrient_per_unit, na.rm = TRUE),
      sd = sd(nutrient_per_unit, na.rm = TRUE)
    ) 
  

  

  # Add in data about how many products in each cell we have data for
  nutrient_per_unit <- 
    nutrient_per_unit %>% 
    left_join(
      df %>% 
        filter(
          department_descr == department & !is.na(nutrient)
        ) %>% 
        count(product_module_descr, units_new)
    )
  

  
  # Filter out cells that have less than 2 products
  nutrient_per_unit <-
    nutrient_per_unit %>% 
    filter(n > 1)
  
  
  
  df <-
    df %>% 
    left_join(
      nutrient_per_unit %>% 
        dplyr::select(
          product_module_descr,
          units_new,
          median_nutr
        ) %>% 
        ungroup(),
      by = c("product_module_descr", "units_new")
    )
  

  df <-
    df %>%
    mutate(
      nutrient =
        case_when(
          !is.na(nutrient) == TRUE ~ nutrient,
          is.na(nutrient) == TRUE ~ median_nutr * size_new
        )
    )
  
  return(df)
}


  

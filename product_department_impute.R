source(paste0(machine_path, "research/projects/niel/nielsen_analysis/product_department_impute_fn.R"))

department_impute_fn <- function(department, products_master, li_upc_update){
  calories <- 
    department_nutrition_impute_fn(products_master, "calories", department, li_upc_update) %>% 
    dplyr::select(
      product_module_descr,
      upc,
      upc_ver_uc,
      calories = nutrient
    )
  
  sf <- 
    department_nutrition_impute_fn(products_master, "saturated_fat", department, li_upc_update) %>% 
    dplyr::select(
      product_module_descr,
      upc,
      upc_ver_uc,
      sf = nutrient
    )
  
  
  sodium <- 
    department_nutrition_impute_fn(products_master, "sodium", department, li_upc_update) %>% 
    dplyr::select(
      product_module_descr,
      upc,
      upc_ver_uc,
      sodium = nutrient
    )
  
  sugar <- 
    department_nutrition_impute_fn(products_master, "sugar", department, li_upc_update) %>% 
    dplyr::select(
      product_module_descr,
      upc,
      upc_ver_uc,
      sodium = nutrient
    )
  
  as <- 
    department_nutrition_impute_fn(products_master, "add_sugars", department, li_upc_update) %>% 
    dplyr::select(
      product_module_descr,
      upc,
      upc_ver_uc,
      as = nutrient
    )
  
  nf_impute <- 
    calories %>% 
    left_join(
      sf
    ) %>% 
    left_join(
      sodium
    ) %>% 
    left_join(
      as
    )
  
  return(nf_impute)
}

dairy_nf_impute <- department_impute_fn("DAIRY", products_master, li_upc_update)
deli_nf_impute <- department_impute_fn("DELI", products_master, li_upc_update)
dry_grocery_nf_impute <- department_impute_fn("DRY GROCERY", products_master, li_upc_update)
fresh_produce_nf_impute <- department_impute_fn("FRESH PRODUCE", products_master, li_upc_update)
frozen_foods_nf_impute <- department_impute_fn("FROZEN FOODS", products_master, li_upc_update)
packaged_meat_nf_impute <- department_impute_fn("PACKAGED MEAT", products_master, li_upc_update)

nf_impute <-
  bind_rows(
    dairy_nf_impute,
    deli_nf_impute,
    dry_grocery_nf_impute,
    fresh_produce_nf_impute,
    frozen_foods_nf_impute,
    packaged_meat_nf_impute
  )

write_csv(nf_impute, paste0(machine_path, "research/projects/niel/nielsen_data_output/products_master_nf_impute.csv"))

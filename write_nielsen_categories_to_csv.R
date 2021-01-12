product_groups <- 
  products_master %>% 
  dplyr::select(product_group_descr) %>% 
  distinct()

write_csv(product_groups, paste0(machine_path, "research/projects/niel/product_groups.csv"))

product_modules <- 
  products_master %>% 
  filter(
    department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT", NA)
  ) %>% 
  dplyr::select(department_descr, product_module_descr) %>% 
  distinct(., .keep_all = TRUE)

write_csv(product_modules, paste0(machine_path, "research/projects/niel/product_modules.csv"))

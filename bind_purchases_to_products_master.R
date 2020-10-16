products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

niel_df <-
  niel_df %>% 
  left_join(
    products_master %>% 
      dplyr::select(
        product_group_descr,
        product_module_descr,
        department_descr,
        # class, Daniel created this
        upc,
        upc_ver_uc,
        # upc_new
      ),
    by = c("upc", "upc_ver_uc")
  )

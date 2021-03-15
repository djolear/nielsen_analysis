upc_descr <-
  products_master %>% 
  filter(str_detect(product_module_descr, "REFERENCE CARD")) %>% 
  count(department_descr, upc_descr)

write_csv(upc_descr, "G:/My Drive/research/projects/niel/nielsen_analysis/reference_card_analysis/reference_card_items.csv")

produce <-
  niel_df %>% 
  filter(department_descr == "FRESH PRODUCE") %>% 
  sample_n(1000) %>% 
  count(product_module_descr, upc_descr)

write_csv(produce, "G:/My Drive/research/projects/niel/nielsen_analysis/reference_card_analysis/fresh_produce.csv")

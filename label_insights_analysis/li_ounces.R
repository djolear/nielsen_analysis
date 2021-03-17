li_ounces <-
  li_upc %>% 
  mutate(
    servings = gsub("[^0-9.]", "", `Serves Per Pack`),
    new_servings = ifelse(as.numeric(servings) >= 1, as.numeric(servings), NA),
    
    size = Size,
    size_quant = as.numeric(gsub("[^0-9.]", "", Size)),
    size_uom = str_to_lower(str_extract(Size, "[:alpha:]{1,}"))
  ) %>% 
  filter(size_uom == "oz") %>% 
  mutate(size_oz = size_quant) %>% 
  filter(!is.na(new_servings)) %>% 
  mutate(
    calories_sc = Calories * new_servings
  )

write_csv(li_ounces, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_oz_only_021721.csv")

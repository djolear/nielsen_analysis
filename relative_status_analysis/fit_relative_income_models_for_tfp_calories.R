sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )




fit_models <- function(df, median_income_var_name, year){
  
  df <-
    df %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      income_scale, 
      median_monthly_housing_cost_county_scale,
      land_area_2010_scale,
      total_pop_county_scale,
      Race,
      Male_Head_Education_scale,
      Female_Head_Education_scale,
      Male_Head_Age_scale,
      Female_Head_Age_scale,
      Male_Head_Employment,
      Female_Head_Employment,
      Marital_Status,
      Household_Size_scale,
      starts_with("tfp"),
      year,
      fips_code
    ) %>% 
    filter_at(
      vars(
        median_income_var_scale ,
        income_scale, 
        median_monthly_housing_cost_county_scale,
        land_area_2010_scale,
        total_pop_county_scale,
        Race,
        Male_Head_Education_scale,
        Female_Head_Education_scale,
        Male_Head_Age_scale,
        Female_Head_Age_scale,
        Male_Head_Employment,
        Female_Head_Employment,
        Marital_Status,
        Household_Size_scale,
        starts_with("tfp")
      ),
      all_vars(!is.na(.))
    ) %>% 
    mutate_at(
      vars(
        Race,
        Male_Head_Employment,
        Female_Head_Employment,
        Marital_Status,
        year,
        fips_code
      ),
      as.factor
    )
  
  df <-
    df %>% 
    gather(key, value, tfp_1:tfp_9) 
  
  df <-
    df %>% 
    mutate(
      value = ifelse(is.na(value), 0, value)
    )
  
  models <- 
    df %>% 
    group_by(key) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~lm(
            scale(value) ~ 
              income_scale + 
              median_income_var_scale +
              median_monthly_housing_cost_county_scale +
              land_area_2010_scale +
              total_pop_county_scale +
              Male_Head_Education_scale +
              Female_Head_Education_scale +
              Male_Head_Age_scale +
              Female_Head_Age_scale +
              Race +
              Male_Head_Employment +
              Female_Head_Employment +
              Marital_Status +
              Household_Size_scale, 
            data = .
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1)) %>% 
    mutate(
      year = year,
      int = "no",
      med_inc_var =  median_income_var_name
    )
  
  models_int <- 
    df %>% 
    group_by(key) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~lm(
            scale(value) ~ 
              income_scale +
              median_income_var_scale * Male_Head_Education_scale +
              median_income_var_scale * Female_Head_Education_scale +
              median_income_var_scale * Male_Head_Age_scale +
              median_income_var_scale * Female_Head_Age_scale +
              median_income_var_scale * Male_Head_Employment +
              median_income_var_scale * Female_Head_Employment +
              median_income_var_scale * Race +
              median_monthly_housing_cost_county_scale +
              land_area_2010_scale +
              total_pop_county_scale +
              Male_Head_Age_scale +
              Female_Head_Age_scale +
              Race +
              Male_Head_Employment +
              Female_Head_Employment +
              Marital_Status +
              Household_Size_scale,
            data = .
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1)) %>% 
    mutate(
      year = year,
      int = "yes",
      med_inc_var =  median_income_var_name
    )
  
  res <-
    bind_rows(
      models_int,
      models
    )
  
  return(res)
}


master_function <- function(path) {
  
  df <- read_csv(paste0(data_path, path))
  
  med_inc_vars <-
    c("median_income_county_scale", "med_inc_niel_male_scale", "med_inc_niel_female_scale",  "med_inc_gen_niel_scale")
  
  res <- 
    map_dfr(.x = med_inc_vars, .f = fit_models, df = df, year =  str_extract(path, "[[:digit:]]+"))
  
  return(res)
}



data_path <- "D:/data/nielsen/tfp_calories_imputed_sc_by_household_quarterly/"

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, "secondary")
  ) 

res <- map_dfr(.x = file_list$file_list, .f = master_function)

write_csv(res, paste0(data_path, "tfp_calories_imputed_sc_by_household_quarterly_regression_results.csv"))

# 
# 
# res <- fit_models(df, "med_inc_gen_niel_scale", 2005)
# 
# res %>% filter(int == "no") %>% dplyr::select(median_income_var_scale) %>% arrange((median_income_var_scale))





load_movers_data <- function(year){
  
  year1 = year
  year2 = year - 2
  
  df1 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year1, ".csv"))
  
  df2 <-
    read_csv(paste0("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_", year2, ".csv"))

  
  df <-
    df1 %>% 
    left_join(
      df2,
      by = c("household_code", "quarter"),
      suffix = c("_t2", "_t1")
    )
  
  movers <-
    df %>% 
    filter(zip.x != zip.y) 
  
  return(movers)  
}



fit_movers <- function(data) {
  lm1 <-
    lmer(
      scale(yes_scale.x) ~
        scale(yes_scale.y) +
        
        scale(I(median_income_county.x - median_income_county.y)) +
        median_income_county_scale.y +
        
        scale(I(physicians.x - physicians.y)) +
        scale(physicians.y) +
        
        scale(I(dentists.x - dentists.y)) +
        scale(dentists.y) +
        
        scale(I(therapists.x - therapists.y)) +
        scale(therapists.y) +
        
        # scale(I(median_income_county.x - median_income_county.y)) +
        # median_income_county_scale.y +
        
        scale(I(median_monthly_housing_cost_county.x - median_monthly_housing_cost_county.y)) +
        median_monthly_housing_cost_county_scale.y +
        
        scale(I(income.x - income.y)) +
        income_scale.y +
        
        scale(I(Male_Head_Education.x - Male_Head_Education.y)) +
        Male_Head_Education_scale.y +
        
        scale(I(Female_Head_Education.x - Female_Head_Education.y)) +
        Female_Head_Education_scale.y +
        
        scale(I(Male_Head_Age.x - Male_Head_Age.y)) +
        Male_Head_Age_scale.y +
        
        scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
        Female_Head_Age_scale.y +
        
        scale(I(total_pop_county.x - total_pop_county.y)) +
        total_pop_county_scale.y +
        
        scale(I(land_area_2010.x - land_area_2010.y)) +
        land_area_2010_scale.y +
        
        scale(I(Household_Size.x - Household_Size.y)) +
        Household_Size_scale.y +
        
        Race.y +
        
        Male_Head_Employment.y +
        Female_Head_Employment.y +
        Marital_Status.y +
        
        Male_Head_Employment.x +
        Female_Head_Employment.x +
        Marital_Status.x +
        
        year.x +
        year.y +
        
        (1 + scale(I(median_income_county.x - median_income_county.y))|fips_code.x) +
        (1|fips_code.y) +
        (1|household_code) +
        (1|quarter),
      data =
        all_data %>% 
        mutate_at(
          vars(
            starts_with("phys"),
            starts_with("dent"),
            starts_with("ther")
          ),
          as.numeric
        ) 
    )
  
  
  res <-
    tidy(lm1) %>% 
    filter(term == "scale(I(median_income_county.x - median_income_county.y))") %>% 
    mutate(year = data$year.x[1])
  
  return(res)
}

years <- seq(2006, 2019, 1)

master_function <- function(year){
  data <- load_movers_data(year)
  res <- fit_movers(data)
  return(res)
}

all_data <-
  map_dfr(.x = years, .f = load_movers_data)




bind_county_census_data_function <- function(df, year) {
  
  ########################
  ## Zipcode City Names ##
  ########################
  
  # source(paste0(machine_path, "research/projects/data1/geography/zipcodes_city_names.R"))
  # 
  # nps <-
  #   nps %>% 
  #   left_join(
  #     zipcodes %>% mutate(zip = as.character(zip))
  #   )  
  
  ################
  ## Census SES ##
  ################
  
  year = ifelse(year < 2010, 2010, year)
  year = ifelse(year == 2019, 2018, year)
  
  census <- 
    read_csv(paste0(machine_path, "research/projects/secondary_data/census/census_ses_county_wide_", year, ".csv"))
  
  df <-
    df %>% 
    left_join(
      census %>% 
        dplyr::select(
          fips_code,
          median_income_county = median_income,
          median_monthly_housing_cost_county = median_monthly_housing_cost,
          median_home_value_county = median_home_value,
          gini_county = gini,
          unweighted_pop_county = unweighted_population,
          total_pop_county = total_population
        ),
      by = "fips_code"
    )
  

  ##############
  ## Mobility ##
  ##############
  
  # source(paste0(machine_path, "research/projects/secondary_data/mobility/mobility_county2zip.R"))
  # 
  # nps <-
  #   nps %>% 
  #   left_join(
  #     zip_mob,
  #     by = "zip"
  #   )
  
  ## Land Area ##
  
  county_land_area <- 
    read_csv(paste0(machine_path, "research/projects/secondary_data/general_geography/county_land_area.csv"))
  
  df <-
    df %>% 
    left_join(
      county_land_area %>% 
        dplyr::select(
          fips_code, 
          land_area_2010 
        ),
      by = "fips_code"
    )
  
  return(df)
}















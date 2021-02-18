gallup_select <- function(df, data_year) {
  print(data_year)
  df <-
    df %>% 
    group_by(FIPS_CODE) %>% 
    summarise(
      ladder_now = mean(ladder_now, na.rm = TRUE),
      ladder_5yrs = mean(ladder_5yrs, na.rm = TRUE)
      #wb = mean(WELL_BEING_INDEX, na.rm = TRUE)
    ) %>% 
    mutate(
      cl_diff = ladder_5yrs - ladder_now,
      year = data_year
    )
  
  return(df)
}

years_list <- list("2012", "2013", "2014", "2015")
df_list <- list(dfg_2012, dfg_2013, dfg_2014, dfg_2015)

county_wb <-
  map2_dfr(.x = df_list, .y = years_list, .f = gallup_select)




niel_select <- function(df, data_year) {
  df <-
    df %>% 
    filter(class == "unhealthy") %>% 
    group_by(fips_code) %>% 
    summarise(
      unhealthy = mean(spend_per_cat_per, na.rm = TRUE)
    ) %>% 
    left_join(
      spend_per_cat12 %>% 
        filter(class == "healthy") %>% 
        group_by(fips_code) %>% 
        summarise(
          healthy = mean(spend_per_cat_per, na.rm = TRUE)
        )
    ) %>% 
    mutate(
      year = data_year
    )
  
  return(df)
}

years_list <- list("2012", "2013", "2014", "2015")
df_list <- list(spend_per_cat12, spend_per_cat13, spend_per_cat14, spend_per_cat15)

county_food <-
  map2_dfr(.x = df_list, .y = years_list, .f = niel_select)



county_food_wb <-
  county_food %>% 
  left_join(
    county_wb %>% mutate(fips_code = FIPS_CODE),
    by = c("fips_code", "year")
  )


lm1 <- lm(log(healthy + 1) ~ cl_diff * ladder_now, county_food_wb)
summary(lm1)
plot_model(lm1, type = "int")



## Zip Code

gallup_select <- function(df, data_year) {
  print(data_year)
  df <-
    df %>% 
    group_by(zipcode) %>% 
    summarise(
      ladder_now = mean(ladder_now, na.rm = TRUE),
      ladder_5yrs = mean(ladder_5yrs, na.rm = TRUE)
      #wb = mean(WELL_BEING_INDEX, na.rm = TRUE)
    ) %>% 
    mutate(
      cl_diff = ladder_5yrs - ladder_now,
      year = data_year
    )
  
  return(df)
}

years_list <- list("2012", "2013", "2014", "2015")
df_list <- list(dfg_2012, dfg_2013, dfg_2014, dfg_2015)

zip_wb <-
  map2_dfr(.x = df_list, .y = years_list, .f = gallup_select)




niel_select <- function(df, data_year) {
  df <-
    df %>% 
    filter(class == "unhealthy") %>% 
    mutate(zipcode = Panelist_ZipCd) %>% 
    group_by(zipcode) %>% 
    summarise(
      unhealthy = mean(spend_per_cat_per, na.rm = TRUE)
    ) %>% 
    left_join(
      spend_per_cat12 %>% 
        filter(class == "healthy") %>% 
        mutate(zipcode = Panelist_ZipCd) %>% 
        group_by(zipcode) %>% 
        summarise(
          healthy = mean(spend_per_cat_per, na.rm = TRUE)
        )
    ) %>% 
    mutate(
      year = data_year
    )
  
  return(df)
}

years_list <- list("2012", "2013", "2014", "2015")
df_list <- list(spend_per_cat12, spend_per_cat13, spend_per_cat14, spend_per_cat15)

zip_food <-
  map2_dfr(.x = df_list, .y = years_list, .f = niel_select)



zip_food_wb <-
  zip_food %>% 
  left_join(
    zip_wb,
    by = c("zipcode", "year")
  )


lm1 <- lm(healthy ~ cl_diff * ladder_now, zip_food_wb)
summary(lm1)
plot_model(lm1, type = "int")

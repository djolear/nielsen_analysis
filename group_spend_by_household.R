years <- seq(2013, 2017, 1)

for(i in 1:length(years)) {
  assign(
    paste0("df_wide_", years[i]), 
    read_csv(paste0(machine_path, "research/projects/niel/nielsen_data_output/group_spend_by_household_wide_", years[i], ".csv"))
  )
}

df_wide_2014 <- 
  df_wide_2014 %>% 
  mutate(
    year = 2014
  )

df_wide_2015 <- 
  df_wide_2015 %>% 
  mutate(
    year = 2015
  )

df_wide_2016 <- 
  df_wide_2016 %>% 
  mutate(
    year = 2016
  )

df_wide_2017 <- 
  df_wide_2017 %>% 
  mutate(
    year = 2017
  )

# product_group_income_cors <- 
#   data.frame(cor = cor(df_main[3:ncol(df_main) - 1], df_main$income, use = "pairwise.complete.obs") )
# 
# product_group_income_cors %>% 
#   arrange(desc(cor))


census_ses_zip_wide_2017 <- 
  read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_2017.csv")) %>% 
  dplyr::select(
    zip,
    median_income,
    median_monthly_housing_cost,
    pov_status_below_per,
    gini
  ) %>% 
  mutate(
    year = 2017
  )

census_ses_zip_wide_2016 <- 
  read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_2016.csv")) %>% 
  dplyr::select(
    zip,
    median_income,
    median_monthly_housing_cost,
    pov_status_below_per,
    gini
  ) %>% 
  mutate(
    year = 2016
  )

census_ses_zip_wide_2015 <- 
  read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_2015.csv")) %>% 
  dplyr::select(
    zip,
    median_income,
    median_monthly_housing_cost,
    pov_status_below_per,
    gini
  ) %>% 
  mutate(
    year = 2015
  )

census_ses_zip_wide_2014 <- 
  read_csv(paste0(machine_path, "research/projects/data1/census/census_ses_zipcode_wide_2014.csv")) %>% 
  dplyr::select(
    zip,
    median_income,
    median_monthly_housing_cost,
    pov_status_below_per,
    gini
  ) %>% 
  mutate(
    year = 2014
  )


df_wide_2014 <-
  df_wide_2014 %>% 
  left_join(
    census_ses_zip_wide_2014,
    by = c("year", "zip")
  )

df_wide_2015 <-
  df_wide_2015 %>% 
  left_join(
    census_ses_zip_wide_2015,
    by = c("year", "zip")
  )

df_wide_2016 <-
  df_wide_2016 %>% 
  left_join(
    census_ses_zip_wide_2016,
    by = c("year", "zip")
  )

df_wide_2017 <-
  df_wide_2017 %>% 
  left_join(
    census_ses_zip_wide_2017,
    by = c("year", "zip")
  )




df_main <-
  bind_rows(
    df_wide_2014,
    df_wide_2015,
    df_wide_2016,
    df_wide_2017
  )

df_main <-
  df_main %>% 
  mutate(
    inc_mid = 
      case_when(
        income == 3 ~ 2500,
        income == 4 ~ 6500,
        income == 6 ~ 9000,
        income == 8 ~ 11000,
        income == 10 ~ 13500,
        income == 11 ~ 17500,
        income == 13 ~ 22500,
        income == 15 ~ 27500,
        income == 16 ~ 32500,
        income == 17 ~ 37500,
        income == 18 ~ 42500,
        income == 19 ~ 47500,
        income == 21 ~ 55000,
        income == 23 ~ 65500,
        income == 26 ~ 85000,
        income == 27 ~ 100000
      ),
    inc_diff = inc_mid - median_income
  )

df_main <-
  df_main %>%
  mutate(
    fresh_produce = if_else(is.na(fresh_produce), 0, fresh_produce)
  )
  mutate(
    hei = fresh_produce + nuts  - candy - butter_and_margarine - sugar_sweeteners
  )

library(lme4)


lm1 <-
  lmer(
    fresh_produce ~
      inc_mid + 
      (1|zip) + 
      (1|year),
    data = df_main
  )

summary(lm1)

lm1 <-
  lmer(
    fresh_produce ~
      inc_mid * inc_diff +
      median_monthly_housing_cost +
      pov_status_below_per +
      household_size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip) + 
      (1|year),
    data = df_main
  )

summary(lm1)

lm.beta.lmer(lm1)

sjPlot::plot_model(lm1, term = "inc_diff", type = "pred")
sjPlot::plot_model(lm1, type = "int")

df <-
  tfp_isc_qr_sec_tp %>% 
  dplyr::select(
    income_demo_ranger_sar_scale,
    income_scale, 
    median_home_value_county_scale,
    land_area_2010_scale,
    total_pop_county_scale,
    Race,
    Male_Head_Education_scale,
    Female_Head_Education_scale,
    Male_Head_Age_scale,
    Female_Head_Age_scale,
    Male_Head_Employment_scale,
    Female_Head_Employment_scale,
    Marital_Status,
    household_size_scale,
    starts_with("tfp"),
    year,
    quarter,
    fips_code
  ) %>% 
  filter_at(
    vars(
      income_demo_ranger_sar_scale ,
      income_scale, 
      median_home_value_county_scale,
      land_area_2010_scale,
      total_pop_county_scale,
      Race,
      Male_Head_Education_scale,
      Female_Head_Education_scale,
      Male_Head_Age_scale,
      Female_Head_Age_scale,
      Male_Head_Employment_scale,
      Female_Head_Employment_scale,
      Marital_Status,
      household_size_scale,
      starts_with("tfp")
    ),
    all_vars(!is.na(.))
  ) %>% 
  mutate_at(
    vars(
      Race,
      quarter,
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
            income_demo_ranger_sar_scale +
            median_home_value_county_scale +
            land_area_2010_scale +
            total_pop_county_scale +
            Male_Head_Education_scale +
            Female_Head_Education_scale +
            Male_Head_Age_scale +
            Female_Head_Age_scale +
            Race +
            Male_Head_Employment_scale +
            Female_Head_Employment_scale +
            Marital_Status +
            household_size_scale + 
            year + 
            quarter, 
          data = .
        ) %>%
          tidy %>%
          dplyr::select(term, estimate) %>%
          spread(term, estimate)
      )
  ) %>%  
  dplyr::select(-data) %>% 
  unnest(cols = c(model1))

models <-
  models %>% 
  mutate(
    key = 
      case_when(
        key == "tfp_1" ~ "whole grains",
        key == "tfp_2" ~ "non-whole grains",
        key == "tfp_3" ~ "potato products",
        key == "tfp_4" ~ "dark green veggies",
        key == "tfp_5" ~ "orange veggies",
        key == "tfp_6" ~ "beans, lentils, peas",
        key == "tfp_7" ~ "other veggies",
        key == "tfp_8" ~ "whole fruits",
        key == "tfp_9" ~ "fruit juices",
        key == "tfp_10" ~ "whole milk products",
        key == "tfp_11" ~ "low fat/skim milk products",
        key == "tfp_12" ~ "all cheese",
        key == "tfp_13" ~ "beef, pork, veal, lamb, game",
        key == "tfp_14" ~ "chicken, turkey",
        key == "tfp_15" ~ "fish",
        key == "tfp_16" ~ "bacon, sausage, lunch meats",
        key == "tfp_17" ~ "nuts, nut butters, seeds",
        key == "tfp_18" ~ "eggs, egg mixtures",
        key == "tfp_19" ~ "fats, condiments",
        key == "tfp_20" ~ "coffee, tea",
        key == "tfp_21" ~ "soft drinks, soda",
        key == "tfp_22" ~ "sugar, sweets, candy",
        key == "tfp_23" ~ "soup",
        key == "tfp_24" ~ "frozen entrees"
      )
  )

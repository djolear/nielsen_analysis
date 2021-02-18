sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

data_path <- "research/projects/niel/nielsen_data_output/group_spend_data_all_categories/"
data_path <- "research/projects/niel/nielsen_data_output/group_spend_data_food_only/"
data_path <- "research/projects/niel/nielsen_data_output/"

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(machine_path, data_path))
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, "secondary")
  )

df <- read_csv(paste0(machine_path, data_path, file_list$file_list[2]))

df <-
  df %>% 
  gather(key, value, foodgroup_1:foodgroup_NA) 

df <-
  df %>% 
  mutate(
    value = ifelse(is.na(value), 0, value)
  )

df <-
  df %>% 
  gather(key, value, baby_food_scale:yogurt_scale)

models <- 
  df %>% 
  group_by(key) %>% 
  nest %>%
  mutate(
    model1 = 
      map(
        data, 
        ~lmer(
          value ~ 
            income_scale + 
            scale(med_inc_niel) * Male_Head_Education_scale +
            scale(med_inc_niel) * Female_Head_Education_scale +
            scale(med_inc_niel) * scale(Male_Head_Age) +
            scale(med_inc_niel) * Female_Head_Age_scale +
            scale(med_inc_niel) * Male_Head_Employment +
            scale(med_inc_niel) * Female_Head_Employment +
            scale(med_inc_niel) * Race +
            median_monthly_housing_cost_county_scale +
            land_area_2010_scale +
            total_pop_county_scale +
            scale(Male_Head_Age) +
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
  unnest(cols = c(model1))


models <- 
  df %>% 
  group_by(key) %>% 
  nest %>%
  mutate(
    model1 = 
      map(
        data, 
        ~lmer(
          scale(value) ~ 
            scale(income) + 
            scale(med_inc_niel) * scale(Male_Head_Education) +
            scale(med_inc_niel) * scale(Female_Head_Education) +
            scale(med_inc_niel) * scale(Male_Head_Age) +
            scale(med_inc_niel) * scale(Female_Head_Age) +
            scale(med_inc_niel) * Male_Head_Employment +
            scale(med_inc_niel) * Female_Head_Employment +
            scale(med_inc_niel) * Race +
            scale(median_monthly_housing_cost_county) +
            scale(land_area_2010) +
            scale(total_pop_county) +
            Race +
            Male_Head_Employment +
            Female_Head_Employment +
            Marital_Status +
            scale(Household_Size) +
            (1|household_code) + (1|month), 
          data = .
        ) %>%
          tidy %>%
          dplyr::select(term, estimate) %>%
          spread(term, estimate)
      )
  ) %>%  
  dplyr::select(-data) %>% 
  unnest(cols = c(model1))



lm1 <-
  lm(
    yes_scale ~ 
      med_inc_niel_male_scale +
      med_inc_niel_male_scale * scale(Male_Head_Education) +
      med_inc_niel_male_scale * scale(Female_Head_Education) +
      med_inc_niel_male_scale * scale(Male_Head_Age) +
      med_inc_niel_male_scale * scale(Female_Head_Age) +
      med_inc_niel_male_scale * Male_Head_Employment +
      med_inc_niel_male_scale * Female_Head_Employment +
      med_inc_niel_male_scale * Race +
      income_scale + 
      scale(Male_Head_Education) +
      scale(Female_Head_Education) +
      scale(Male_Head_Age) +
      scale(Female_Head_Age) +
      Male_Head_Employment +
      Female_Head_Employment +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Male_Head_Employment +
      Female_Head_Employment +
      Marital_Status +
      scale(Household_Size), 
  data = qh_calories_imputed_sc_by_household_quarterly_2010
)

summary(lm1)

lm1 <-
  lm(
    yes_scale ~ 
      med_inc_niel_female_scale +
      income_scale + 
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale +
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Male_Head_Employment +
      Female_Head_Employment +
      Marital_Status +
      Household_Size_scale, 
    data = qh_calories_imputed_sc_by_household_quarterly_2005
  )

summary(lm1)

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

file_list <- 
  data.frame(
    file_list = list.files(path = paste0(machine_path, data_path))
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, "secondary")
  )

df <- read_csv(paste0(machine_path, data_path, file_list$file_list[9]))

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
        ~lm(
          value ~ 
            income_scale + 
            med_inc_state_niel_scale * Male_Head_Education_scale * scale(Male_Head_Age) +
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



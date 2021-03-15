single_parent_median_income <- function(df, year){
  
  df <-
    df %>% 
    mutate(
      male_educ_new =
        case_when(
          Female_Head_Age == 0 & Male_Head_Education %in% c(1, 2) ~ 1,
          Female_Head_Age == 0 & Male_Head_Education %in% c(3, 4) ~ 2,
          Female_Head_Age == 0 & Male_Head_Education == 5 ~ 3,
          Female_Head_Age == 0 & Male_Head_Education == 6 ~ 4
        ),
      female_educ_new =
        case_when(
          Male_Head_Age == 0 & Female_Head_Education %in% c(1, 2) ~ 1,
          Male_Head_Age == 0 & Female_Head_Education %in% c(3, 4) ~ 2,
          Male_Head_Age == 0 & Female_Head_Education == 5 ~ 3,
          Male_Head_Age == 0 & Female_Head_Education == 6 ~ 4
        ),
      male_age_new = 
        case_when(
          Female_Head_Age == 0 & Male_Head_Age %in% c(1, 2) ~ 1,
          Female_Head_Age == 0 & Male_Head_Age %in% c(3, 4) ~ 2,
          Female_Head_Age == 0 & Male_Head_Age %in% c(5, 6) ~ 3,
          Female_Head_Age == 0 & Male_Head_Age %in% c(7, 8) ~ 4,
          Female_Head_Age == 0 & Male_Head_Age == 9 ~ 5
        ),
      female_age_new = 
        case_when(
          Male_Head_Age == 0 & Female_Head_Age %in% c(1, 2) ~ 1,
          Male_Head_Age == 0 & Female_Head_Age %in% c(3, 4) ~ 2,
          Male_Head_Age == 0 & Female_Head_Age %in% c(5, 6) ~ 3,
          Male_Head_Age == 0 & Female_Head_Age %in% c(7, 8) ~ 4,
          Male_Head_Age == 0 & Female_Head_Age == 9 ~ 5
        )
    ) 
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  # panelists <-
  #   readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  
  panelists <-
    panelists %>% 
    mutate(
      male_educ_new =
        case_when(
          Female_Head_Age == 0 & Male_Head_Education %in% c(1, 2) ~ 1,
          Female_Head_Age == 0 & Male_Head_Education %in% c(3, 4) ~ 2,
          Female_Head_Age == 0 & Male_Head_Education == 5 ~ 3,
          Female_Head_Age == 0 & Male_Head_Education == 6 ~ 4
        ),
      female_educ_new =
        case_when(
          Male_Head_Age == 0 & Female_Head_Education %in% c(1, 2) ~ 1,
          Male_Head_Age == 0 & Female_Head_Education %in% c(3, 4) ~ 2,
          Male_Head_Age == 0 & Female_Head_Education == 5 ~ 3,
          Male_Head_Age == 0 & Female_Head_Education == 6 ~ 4
        ),
      male_age_new = 
        case_when(
          Female_Head_Age == 0 & Male_Head_Age %in% c(1, 2) ~ 1,
          Female_Head_Age == 0 & Male_Head_Age %in% c(3, 4) ~ 2,
          Female_Head_Age == 0 & Male_Head_Age %in% c(5, 6) ~ 3,
          Female_Head_Age == 0 & Male_Head_Age %in% c(7, 8) ~ 4,
          Female_Head_Age == 0 & Male_Head_Age == 9 ~ 5
        ),
      female_age_new = 
        case_when(
          Male_Head_Age == 0 & Female_Head_Age %in% c(1, 2) ~ 1,
          Male_Head_Age == 0 & Female_Head_Age %in% c(3, 4) ~ 2,
          Male_Head_Age == 0 & Female_Head_Age %in% c(5, 6) ~ 3,
          Male_Head_Age == 0 & Female_Head_Age %in% c(7, 8) ~ 4,
          Male_Head_Age == 0 & Female_Head_Age == 9 ~ 5
        )
    ) 
  
  
  med_inc_niel_female_single <-
    panelists %>% 
    filter(Male_Head_Age == 0) %>% 
    filter(Female_Head_Age > 0) %>% 
    group_by(female_age_new, female_educ_new) %>% 
    summarise(
      med_inc_niel_female_single = matrixStats::weightedMedian(Household_Income, na.rm = TRUE, weight = Projection_Factor)
    ) %>% 
    ungroup() %>% 
    mutate(
      med_inc_niel_female_single_scale = as.numeric(scale(med_inc_niel_female_single))
    )
  
  med_inc_niel_male_single <-
    panelists %>% 
    filter(Female_Head_Age == 0) %>% 
    filter(Male_Head_Age > 0) %>% 
    group_by(male_age_new, male_educ_new) %>% 
    summarise(
      med_inc_niel_male_single = matrixStats::weightedMedian(Household_Income, na.rm = TRUE, weight = Projection_Factor)
    ) %>% 
    ungroup() %>% 
    mutate(
      med_inc_niel_male_single_scale = as.numeric(scale(med_inc_niel_male_single))
    )
  
  df <-
    df %>% 
    left_join(
      med_inc_niel_female_single
    ) %>% 
    left_join(
      med_inc_niel_male_single
    )
  
  return(df)
  
}


median_income_nielsen_general_function <- function(df, year){
  
  panelists <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  med_inc_niel <-
    panelists %>% 
    filter(Male_Head_Age > 0 & Female_Head_Age > 0) %>% 
    group_by(Female_Head_Education, Male_Head_Education, Male_Head_Age, Female_Head_Age) %>% 
    summarise(
      med_inc_gen_niel = matrixStats::weightedMedian(Household_Income, w = Projection_Factor, na.rm = TRUE)
    ) %>% 
    ungroup() 
  
  df <-
    df %>% 
    left_join(
      med_inc_niel,
      by = c("Female_Head_Education", "Male_Head_Education", "Male_Head_Age", "Female_Head_Age")
    )
  
  med_inc_niel_male <-
    panelists %>% 
    filter(Male_Head_Age > 0) %>% 
    group_by(Male_Head_Age, Male_Head_Education) %>% 
    summarise(
      med_inc_niel_male = matrixStats::weightedMedian(Household_Income, na.rm = TRUE, weight = Projection_Factor)
    ) %>% 
    ungroup() 
  
  df <-
    df %>% 
    left_join(
      med_inc_niel_male
    )
  
  
  med_inc_niel_female <-
    panelists %>% 
    filter(Female_Head_Age > 0) %>% 
    group_by(Female_Head_Age, Female_Head_Education) %>% 
    summarise(
      med_inc_niel_female = matrixStats::weightedMedian(Household_Income, na.rm = TRUE, weight = Projection_Factor)
    ) %>% 
    ungroup() 
  
  df <-
    df %>% 
    left_join(
      med_inc_niel_female
    )
  
  df <-
    df %>% 
    mutate(
      med_inc_niel_avg = (med_inc_niel_female + med_inc_niel_male) / 2
    )
  
  
  return(df)
}

median_income_nielsen_state_function <- function(df, year){
  
  panelists <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  panelists <-
    panelists %>% 
    mutate(
      state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
      fips_code = paste0(state_fips, cty_fips)
    )
  
  med_inc_niel <-
    panelists %>% 
    filter(Male_Head_Age > 0 & Female_Head_Age > 0) %>% 
    group_by(Female_Head_Education, Male_Head_Education, state_fips) %>% 
    summarise(
      med_inc_state_niel = matrixStats::weightedMedian(Household_Income, w = Projection_Factor, na.rm = TRUE)
    ) %>% 
    ungroup() 
  
  df <-
    df %>% 
    left_join(
      med_inc_niel,
      by = c("Female_Head_Education", "Male_Head_Education", "state_fips")
    )
  
  return(df)
}


median_income_nielsen_county_function <- function(df, year){
  
  panelists <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  panelists <-
    panelists %>% 
    mutate(
      state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
      fips_code = paste0(state_fips, cty_fips)
    )
  
  med_inc_niel <-
    panelists %>% 
    filter(Male_Head_Age > 0 & Female_Head_Age > 0) %>% 
    group_by(Female_Head_Education, Male_Head_Education, fips_code) %>% 
    summarise(
      med_inc_county_niel = matrixStats::weightedMedian(Household_Income, w = Projection_Factor, na.rm = TRUE)
    ) %>% 
    ungroup() 
  
  df <-
    df %>% 
    left_join(
      med_inc_niel,
      by = c("Female_Head_Education", "Male_Head_Education", "fips_code")
    )
  
  return(df)
}

median_income_nielsen_all_function <- function(df){
  df <- median_income_nielsen_general_function(df, df$year[1])
  df <- median_income_nielsen_state_function(df, df$year[1])
  df <- median_income_nielsen_county_function(df, df$year[1])
  return(df)
}

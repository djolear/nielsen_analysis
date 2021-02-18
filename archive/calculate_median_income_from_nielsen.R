sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

year = 2009


panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

df <-
  df %>% 
  left_join(
    panelists %>% 
      dplyr::select(
        household_code = Household_Cd,
        Fips_State_Cd,
        Projection_Factor
      )
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


med_inc_niel <-
  panelists %>% 
  filter(Female_Head_Age > 0 & Male_Head_Age > 0) %>% 
  group_by(Female_Head_Age, Female_Head_Education, Male_Head_Age, Male_Head_Education) %>% 
  summarise(
    med_inc_niel = matrixStats::weightedMedian(Household_Income, na.rm = TRUE, weight = Projection_Factor)
  ) %>% 
  ungroup() 

df <-
  df %>% 
  left_join(
    med_inc_niel
  )


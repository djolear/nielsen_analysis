sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

year = 2017


panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

df_wide_2017 <-
  df_wide_2017 %>% 
  left_join(
    panelists %>% 
      dplyr::select(
        household_code = Household_Cd,
        Fips_State_Cd
      )
  )


med_inc_niel <-
  panelists %>% 
  filter(Male_Head_Age > 0) %>% 
  group_by(Male_Head_Age, Male_Head_Education) %>% 
  summarise(
    med_inc_niel = median(Household_Income, na.rm = TRUE)
  ) %>% 
  ungroup() 

df_wide_2017 <-
  df_wide_2017 %>% 
  left_join(
    med_inc_niel
  )

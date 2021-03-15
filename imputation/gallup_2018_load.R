df1 <- read_dta("D:/data/gallup/raw/US_TRACKER_2018_DATA_WB_STATA and SPSS/US_WB_DAILY_2018_DATA.dta")
df2 <- read_dta("D:/data/gallup/raw/US_TRACKER_2018_DATA_PE_STATA and SPSS/US_PE_DAILY_2018_DATA.dta")

df <-
  df1 %>% 
  left_join(
    df2,
    by = "MOTHERLODE_ID"
  ) %>% 
  mutate(
    year = 2018
  )

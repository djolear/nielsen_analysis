panelists <-
  panelists %>% 
  mutate(
    inc_mid = 
      case_when(
        Household_Income == 3 ~ 2500,
        Household_Income == 4 ~ 6500,
        Household_Income == 6 ~ 9000,
        Household_Income == 8 ~ 11000,
        Household_Income == 10 ~ 13500,
        Household_Income == 11 ~ 17500,
        Household_Income == 13 ~ 22500,
        Household_Income == 15 ~ 27500,
        Household_Income == 16 ~ 32500,
        Household_Income == 17 ~ 37500,
        Household_Income == 18 ~ 42500,
        Household_Income == 19 ~ 47500,
        Household_Income == 21 ~ 55000,
        Household_Income == 23 ~ 65500,
        Household_Income == 26 ~ 85000,
        Household_Income == 27 ~ 100000
      )
  )



median_earnings_male_county_census_function <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  med_earn <- 
  get_acs(
    geography = "county",
    variables = 
      c(
        male_lths = "B20004_008",
        male_hs = "B20004_009",
        male_sc = "B20004_010",
        male_c = "B20004_011",
        male_g = "B20004_012",
        female_lths = "B20004_014",
        female_hs = "B20004_015",
        female_sc = "B20004_016",
        female_c = "B20004_017",
        female_g = "B20004_018"        
      ),
    year = year
  )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      fips_code = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_cty_male = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Male_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Male_Head_Education == 2, 1, Male_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 2),
      by = c("fips_code", "education_new")
    )

  
  return(df)
}


median_earnings_male_state_census_function <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  med_earn <- 
    get_acs(
      geography = "state",
      variables = 
        c(
          male_lths = "B20004_008",
          male_hs = "B20004_009",
          male_sc = "B20004_010",
          male_c = "B20004_011",
          male_g = "B20004_012",
          female_lths = "B20004_014",
          female_hs = "B20004_015",
          female_sc = "B20004_016",
          female_c = "B20004_017",
          female_g = "B20004_018"        
        ),
      year = year
    )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      state_fips = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_state_male = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Male_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Male_Head_Education == 2, 1, Male_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 2),
      by = c("state_fips", "education_new")
    )
    
  
  return(df)
}


median_earnings_male_zip_census_function <- function(df, year){
  
  year = ifelse(year < 2011, 2011, year)
  
  med_earn <- 
    get_acs(
      geography = "zcta",
      variables = 
        c(
          male_lths = "B20004_008",
          male_hs = "B20004_009",
          male_sc = "B20004_010",
          male_c = "B20004_011",
          male_g = "B20004_012",
          female_lths = "B20004_014",
          female_hs = "B20004_015",
          female_sc = "B20004_016",
          female_c = "B20004_017",
          female_g = "B20004_018"        
        ),
      year = year
    )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      zip = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_zip_male = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Male_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Male_Head_Education == 2, 1, Male_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 2),
      by = c("zip", "education_new")
    )
  
  return(df)
}

median_earnings_female_county_census_function <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  med_earn <- 
    get_acs(
      geography = "county",
      variables = 
        c(
          male_lths = "B20004_008",
          male_hs = "B20004_009",
          male_sc = "B20004_010",
          male_c = "B20004_011",
          male_g = "B20004_012",
          female_lths = "B20004_014",
          female_hs = "B20004_015",
          female_sc = "B20004_016",
          female_c = "B20004_017",
          female_g = "B20004_018"        
        ),
      year = year
    )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      fips_code = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_cty_female = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Female_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Female_Head_Education == 2, 1, Female_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 1),
      by = c("fips_code", "education_new")
    )
  
  
  return(df)
}


median_earnings_female_state_census_function <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  med_earn <- 
    get_acs(
      geography = "state",
      variables = 
        c(
          male_lths = "B20004_008",
          male_hs = "B20004_009",
          male_sc = "B20004_010",
          male_c = "B20004_011",
          male_g = "B20004_012",
          female_lths = "B20004_014",
          female_hs = "B20004_015",
          female_sc = "B20004_016",
          female_c = "B20004_017",
          female_g = "B20004_018"        
        ),
      year = year
    )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      state_fips = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_state_female = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Female_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Female_Head_Education == 2, 1, Female_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 1),
      by = c("state_fips", "education_new")
    )
  
  
  return(df)
}


median_earnings_female_zip_census_function <- function(df, year){
  
  year = ifelse(year < 2011, 2011, year)
  
  med_earn <- 
    get_acs(
      geography = "zcta",
      variables = 
        c(
          male_lths = "B20004_008",
          male_hs = "B20004_009",
          male_sc = "B20004_010",
          male_c = "B20004_011",
          male_g = "B20004_012",
          female_lths = "B20004_014",
          female_hs = "B20004_015",
          female_sc = "B20004_016",
          female_c = "B20004_017",
          female_g = "B20004_018"        
        ),
      year = year
    )
  
  med_earn <-
    med_earn %>% 
    dplyr::select(
      zip = GEOID, 
      variable, 
      estimate
    ) %>% 
    separate(variable, into = c("sex", "education"), sep = "_") %>% 
    mutate(
      med_earn_census_zip_female = estimate,
      sex = 
        case_when(
          sex == "male" ~ 1,
          sex == "female" ~ 2
        ),
      education_new = 
        case_when(
          education == "lths" ~ 1,
          education == "hs" ~ 3,
          education == "sc" ~ 4,
          education == "c" ~ 5,
          education == "g" ~ 6,
        )
    ) %>% 
    dplyr::select(-c(education, estimate))
  
  df <-
    df %>% 
    filter(
      Female_Head_Education > 0
    ) %>% 
    mutate(
      education_new = ifelse(Female_Head_Education == 2, 1, Female_Head_Education)
    ) %>% 
    left_join(
      med_earn %>% filter(sex != 1),
      by = c("zip", "education_new")
    )
  
  return(df)
}


median_earnings_all_census_function <- function(df){
  df <- median_earnings_male_county_census_function(df, df$year[1])
  df <- median_earnings_male_state_census_function(df, df$year[1])
  #df <- median_earnings_male_zip_census_function(df, df$year[1])
  df <- median_earnings_female_county_census_function(df, df$year[1])
  df <- median_earnings_female_state_census_function(df, df$year[1])
  #df <- median_earnings_female_zip_census_function(df, df$year[1])
  gc()
  return(df)
  
}



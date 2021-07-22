read_panelists <- function(year){
  df <-
    read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv" )) %>% 
    mutate(year = year)
    
  return(df)
}

years <-
  c(2004:2019)

panelists <-
  map_dfr(years, read_panelists)

data <-
  panelists %>%
  dplyr::select(
    Household_Cd,
    Male_Head_Age,
    Female_Head_Age,
    Race,
    Hispanic_Origin,
    Household_Income,
    year,
    Projection_Factor
  ) %>% 
  filter(Male_Head_Age != 0 & Female_Head_Age != 0) %>% 
  filter_at(
    vars(
      Household_Cd,
      Male_Head_Age,
      Female_Head_Age,
      Race,
      Hispanic_Origin,
      Household_Income,
      year
    ),
    all_vars(!is.na(.))
  ) %>% 
  distinct() %>% 
  mutate_at(
    vars(
      Race,
      Hispanic_Origin,
      year
    ),
    as.factor
  )


data_train <-
  data %>% 
  dplyr::select(
    Male_Head_Age,
    Female_Head_Age,
    Race,
    Hispanic_Origin,
    Household_Income,
    year,
    Projection_Factor
  )
  



panelists_split <-
  initial_split(data_train)

panelists_train <- training(panelists_split)
panelists_test <- testing(panelists_split)
val_set <- validation_split(panelists_train, prop = 0.8)

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = 4) %>% 
  set_mode("regression")


rf_recipe <- 
  recipe(
    Household_Income ~ Male_Head_Age + Female_Head_Age + Race + Hispanic_Origin + year, 
    data = panelists_train, 
    case.weights = panelists_train$Projection_Factor
  )

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

set.seed(345)

rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))


autoplot(rf_res)

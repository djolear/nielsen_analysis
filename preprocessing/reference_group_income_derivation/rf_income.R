###################
## Load Packages ##
###################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  ranger,
  caret,
  tidymodels,
  broom,
  broom.mixed,
  gbm
)

###############
## Load Data ##
###############

nielsen_calorie_budget <-
  read_csv("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_all_years.csv")

years <- c(2004:2019)

read_panelists <- function(year){
  path <- paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv")
  df <- read_tsv(path) %>% mutate(year)
  return(df)
}

panelists_master <- 
  map_dfr(.x = years, .f = read_panelists)
  


data <-
  panelists_master %>%
  dplyr::select(
    household_code = Household_Cd,
    Male_Head_Age,
    Female_Head_Age,
    Race,
    income = Household_Income,
    Projection_Factor,
    year
  ) %>% 
  filter_at(
    vars(
      household_code,
      Male_Head_Age,
      Female_Head_Age,
      Race,
      income,
      Projection_Factor,
      year
    ),
    all_vars(!is.na(.))
  ) %>% 
  distinct() %>% 
  mutate_at(
    vars(
      contains("Age"),
      Race,
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
    income,
    year
  )

rf <- 
  ranger(
    income ~ .,
    data = data_train,
    case.weights = data$Projection_Factor
  )

data <-
  bind_cols(
    data,
    income_demo_ranger_sar_scale = as.numeric(scale(rf$predictions))
  )

write_csv(data, "D:/data/nielsen/ml/rf_income_predictions_default_070721.csv")

# 
# 
# 
# data_train <-
#   data %>% 
#   dplyr::select(
#     Male_Head_Age,
#     Female_Head_Age,
#     Male_Head_Education,
#     Female_Head_Education,
#     Male_Head_Employment,
#     Female_Head_Employment,
#     Marital_Status,
#     median_home_value_county_scale,
#     land_area_2010_scale,
#     total_pop_county_scale,
#     Race,
#     income,
#     # median_income_demo_nielsen_esa_scale,
#     # median_income_demo_nielsen_sar_avg_scale,
#     income_demo_ranger...21,
#     income_demo_ranger...23
#   ) 
# 
# outcome_train <-
#   data %>% 
#   dplyr::select(
#     yes_scale
#   ) %>%
#   data.matrix()
# 
# x_train <- model.matrix( ~ .-1, data_train)
# x_train
# 
# 
# # rf <- 
# #   ranger(
# #     yes_scale ~ .,
# #     data = data_train,
# #     importance = "impurity"
# #   )
# # 
# # importance(rf)
# 
# library(glmnet)
# 
# set.seed(123) 
# cv <- cv.glmnet(x = data.matrix(x_train), y = outcome_train)
# # Display the best lambda value
# cv$lambda.min
# 
# 
# model <- glmnet(x = x_train, y = outcome_train, alpha = 1, lambda = cv$lambda.min)
# 
# coef(model)
# 
# library(caret)
# cv_5 = trainControl(method = "cv", number = 5)
# 
# hit_elnet = train(
#   x = data_train, y = as.numeric(outcome_train),
#   method = "glmnet",
#   trControl = cv_5
# )
# 
# get_best_result = function(caret_fit) {
#   best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
#   best_result = caret_fit$results[best, ]
#   rownames(best_result) = NULL
#   best_result
# }
# 
# get_best_result(hit_elnet)
# 
# varImp(hit_elnet)
# 
# coef(hit_elnet$finalModel, hit_elnet$finalModel$lambdaOpt)

df <- 
  read_csv("D:/data/nielsen/group_calories_by_household_monthly_syndigo_wide_2017.csv")

# nielsen_calorie_budget <- bind_rows(qh_calories_imputed_sc_by_household_quarterly_2004, qh_calories_imputed_sc_by_household_quarterly_2005, qh_calories_imputed_sc_by_household_quarterly_2006,qh_calories_imputed_sc_by_household_quarterly_2007,qh_calories_imputed_sc_by_household_quarterly_2008, qh_calories_imputed_sc_by_household_quarterly_2009,qh_calories_imputed_sc_by_household_quarterly_2010,qh_calories_imputed_sc_by_household_quarterly_2011,qh_calories_imputed_sc_by_household_quarterly_2012,qh_calories_imputed_sc_by_household_quarterly_2013,qh_calories_imputed_sc_by_household_quarterly_2014,qh_calories_imputed_sc_by_household_quarterly_2015,qh_calories_imputed_sc_by_household_quarterly_2016,qh_calories_imputed_sc_by_household_quarterly_2017,qh_calories_imputed_sc_by_household_quarterly_2018,qh_calories_imputed_sc_by_household_quarterly_2019)

data <-
  nielsen_calorie_budget %>%
  dplyr::select(
    household_code,
    Male_Head_Age,
    Female_Head_Age,
    Race,
    income,
    year
  ) %>% 
  filter_at(
    vars(
      household_code,
      Male_Head_Age,
      Female_Head_Age,
      Race,
      income,
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

# data <-
#   data %>% 
#   mutate(
#     income_quintile = 
#       .bincode(
#         median_income_county, 
#         breaks = quantile(probs = seq(0, 1, 0.2), median_income_county, na.rm = T), 
#         include.lowest = TRUE
#       )
#   )

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
    case.weights = data
  )

data <-
  bind_cols(
    data,
    income_demo_ranger_sar_scale = as.numeric(scale(rf$predictions))
  )

nielsen_dollar_budget <-
  nielsen_dollar_budget %>% mutate(Race = as.factor(Race), year = as.factor(year)) %>% 
  left_join(
    data  %>% 
      dplyr::select(household_code, year, income_demo_ranger_sar_scale),
    by = c("household_code", "year")
  )


lm1 <-
  lm(
    yes_scale ~
      income_demo_ranger_sar_scale +
      income +
      Male_Head_Education +
      Female_Head_Education +
      Male_Head_Age +
      Female_Head_Age +
      Male_Head_Employment +
      Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Marital_Status +
      Household_Size_scale + 
      quarter,
    data = 
      nielsen_calorie_budget
  )

summary(lm1)
lm.beta(lm1)
regclass::VIF(lm1)


lm1 <-
  lm(
    yes_scale ~
      median_income_demo_nielsen_esa_scale +
      income +
      Male_Head_Education +
      Female_Head_Education +
      Male_Head_Age +
      Female_Head_Age +
      # Male_Head_Employment +
      # Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      # Marital_Status +
      Household_Size_scale + 
      quarter,
    data = 
      data
  )

summary(lm1)
lm.beta(lm1)
regclass::VIF(lm1)


data_train <-
  data %>% 
  dplyr::select(
    Male_Head_Age,
    Female_Head_Age,
    Male_Head_Education,
    Female_Head_Education,
    Male_Head_Employment,
    Female_Head_Employment,
    Marital_Status,
    median_home_value_county_scale,
    land_area_2010_scale,
    total_pop_county_scale,
    Race,
    income,
    # median_income_demo_nielsen_esa_scale,
    # median_income_demo_nielsen_sar_avg_scale,
    income_demo_ranger...21,
    income_demo_ranger...23
  ) 

outcome_train <-
  data %>% 
  dplyr::select(
    yes_scale
  ) %>%
  data.matrix()

x_train <- model.matrix( ~ .-1, data_train)
x_train


# rf <- 
#   ranger(
#     yes_scale ~ .,
#     data = data_train,
#     importance = "impurity"
#   )
# 
# importance(rf)

library(glmnet)

set.seed(123) 
cv <- cv.glmnet(x = data.matrix(x_train), y = outcome_train)
# Display the best lambda value
cv$lambda.min


model <- glmnet(x = x_train, y = outcome_train, alpha = 1, lambda = cv$lambda.min)

coef(model)

library(caret)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(
  x = data_train, y = as.numeric(outcome_train),
  method = "glmnet",
  trControl = cv_5
)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(hit_elnet)

varImp(hit_elnet)

coef(hit_elnet$finalModel, hit_elnet$finalModel$lambdaOpt)

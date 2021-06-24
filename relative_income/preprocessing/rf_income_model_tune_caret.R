###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/Rpackages")
library("lavaan", lib.loc = "/home/djolear/Rpackages")
library("furrr", lib.loc = "/home/djolear/Rpackages")
library("broom", lib.loc = "/home/djolear/Rpackages")
library("glmnet", lib.loc = "/home/djolear/Rpackages")
library("randomForest", lib.loc = "/home/djolear/Rpackages")
library("ranger", lib.loc = "/home/djolear/Rpackages")
# library("foreach", lib.loc = "/home/djolear/Rpackages")
# library("doParallel", lib.loc = "/home/djolear/Rpackages")
library("caret", lib.loc = "/home/djolear/Rpackages")


## Functions ##

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

## Load Data ##

years <- c(2004:2019)

read_panelists <- function(year) {
  df <- 
    read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv")) %>% 
    mutate(year = year)
  #df <- read_tsv(paste0(" /kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  return(df)
}

df <- map_dfr(.x = years, .f = read_panelists)



# data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"
# 
# data <- 
#   read_rds(data_path)

data <-
  df %>%
  dplyr::select(
    household_code = Household_Cd,
    Male_Head_Age,
    Female_Head_Age,
    Race,
    income = Household_Income,
    year,
    Projection_Factor
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
  mutate(income = as.numeric(income)) %>% 
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
    Male_Head_Age:Race, year, income
  )

## Set up Tune Grid ##

tgrid <-
  expand.grid(
    mtry = c(1, 2, 3),
    splitrule = "variance",
    min.node.size = c(5, 10, 15, 20, 30, 40, 50, 60)
  )


## Parallelize ##

# cl <- makePSOCKcluster(8)
# registerDoParallel(cl)

## Tune Model ##

model_caret <- 
  train(
    x = data_train[, 1:4],
    y = data_train$income,
    method = "ranger",
    trControl = 
      trainControl(
        method="cv", 
        number = 5, 
        verboseIter = T,
        allowParallel = TRUE
      ),
    tuneGrid = tgrid,
    num.trees = 500,
    num.threads = 11,
    weights = data$Projection_Factor
  )

# stopCluster(cl)

## Export Results ##

results <- 
  get_best_result(model_caret)

write_csv(results, "/project/ourminsk/gallup/results/ml/results_rf_income_sar_vars.csv")



preds <- predict(model_caret, data_train)

data <-
  bind_cols(
    data,
    income_demo_ranger_all_vars_scale = scale(preds)
  ) 

data <-
  data %>% 
  dplyr::select(
    subid,
    income_demo_ranger_sar_vars_scale
  )

write_csv(data, "/project/ourminsk/gallup/results/ml/preds_rf_income_sar_vars.csv")


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


## Load Data ##

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

data <- 
  read_rds(data_path)

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
    year
  )


# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = c(2, 3),
  node_size  = c(seq(5, 200, 15), 300, 400, 500),
  sampe_size = c(.632),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)
## [1] 96

for(i in 1:nrow(hyper_grid)) {
  print(i)
  
  # train model
  model <- ranger(
    formula         = Household_Income ~ ., 
    data            = data_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123,
    case.weights    = data$COMB_WEIGHT,
    num.threads     = 8,
    case.weights 
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

write_csv(hyper_grid, "/project/ourminsk/gallup/results/ml/hyper_grid.csv")
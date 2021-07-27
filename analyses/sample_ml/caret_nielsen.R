
## Load Packages ##

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  broom,
  caret,
  ranger,
  glmnet
)


## Partition the Data ##

set.seed(1234)

training_indices <-
  createDataPartition(
    y = data$yes_scale,
    p = 0.75,
    list = FALSE
  )


training_data <- data[ training_indices,]
testing_data  <- data[-training_indices,]

nrow(training_data)
nrow(testing_data)


### ELASTIC NET ###

## Hyperparameter Search ##

# Let's set up our tune grid. I like this one because it covers a really wide range of possibilities.

tune_grid <-
  expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 1, length = 10)
  )

# Let's set up our resampling parameters

# We could use repeated cv, but I don't know that we really need all that firepower for this problem at the moment.
training_control <- 
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    verboseIter = TRUE
  )


# So we'll go with the simpler 10-fold CV
training_control <- 
  trainControl(
    method = "cv",
    number = 10,
  )


# Now we can do the hyperparamter search
elnet_model <- 
  train(
    yes_scale ~ .,
    data = training_data,
    method = "glmnet",
    trControl = training_control,
    tuneGrid = tune_grid
  )


# Define the get best result function
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}


# Get the best model
get_best_result(elnet_model)


### RANDOM FOREST ###

#I'm not going to use this, but we could define our own training grid.
# rf_grid <- 
#   expand.grid(
#     mtry = c(2, 3, 4, 5),
#     splitrule = c("variance", "extratrees"),
#     min.node.size = c(1, 3, 5)
#   )
# 
# rf_grid

rf_model <- 
  train(
    yes_scale ~ .,
    data = training_data,
    method = "ranger",
    trControl = training_control
  )

get_best_result(rf_model)


### GRADIENT BOOSTED MACHINE ###

gbm_model <- 
  train(
    yes_scale ~ .,
    data = training_data,
    method = "gbm",
    trControl = training_control
  )

get_best_result(gbm_model)


###  XGBOOST ###

# tune_grid <- expand.grid(
#   nrounds = seq(from = 200, to = 400, by = 50),
#   eta = c(0.025, 0.05, 0.1, 0.3),
#   max_depth = c(2, 3, 4, 5, 6)
# )

xgb_tune <- caret::train(
  yes_scale ~ .,
  data = training_data,
  trControl = training_control,
  # tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)


get_best_result(xgb_tune)


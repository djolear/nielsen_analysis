

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

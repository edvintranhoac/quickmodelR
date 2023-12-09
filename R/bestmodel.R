# main function
bestmodels <- function(models_list) {
  best <- list()

  # Finding the best model for quantitative models (lowest RMSE)
  quant_models <- c("lm", "knn", "rf", "rpart", "gbm", "glmnet")
  quant_models_list <- Filter(function(model) model$method %in% quant_models, models_list$models)

  if (length(quant_models_list) > 0) {
    # Extract RMSE values for each model in quant_models_list
    rmse_values <- unlist(lapply(quant_models_list, function(model) model$results$RMSE))

    if (length(rmse_values) > 0) {
      # Find the index of the minimum RMSE value
      best_model_index <- which.min(rmse_values)

      # Check which model has the lowest RMSE value
      print(paste("Best RMSE:", rmse_values[best_model_index]))
      print(quant_models_list)

      # Retrieve the best model based on the index
      best_quant_model <- quant_models_list[[best_model_index]]
      print(best_quant_model)
    } else {
      print("No RMSE values found.")
    }
  } else {
    print("No models found for quantitative criteria.")
  }


  # Finding the best model for categorical models (highest Accuracy)
  categ_models <- c("glm", "knn", "rf", "rpart", "gbm", "glmnet")
  categ_models_list <- Filter(function(model) model$method %in% categ_models, models_list$models)

  if (length(categ_models_list) > 0) {
    # Extract Accuracy values for each model in categ_models_list
    accuracy_values <- unlist(lapply(categ_models_list, function(model) model$results$Accuracy))

    if (length(accuracy_values) > 0) {
      # Find the index of the maximum Accuracy value
      best_model_index <- which.max(accuracy_values)

      # Retrieve the best model based on the index
      best_categ_model <- categ_models_list[[best_model_index]]
      best[["categorical"]] <- best_categ_model
    } else {
      best[["categorical"]] <- NULL
    }
  } else {
    best[["categorical"]] <- NULL
  }

  return(best)
}


bestmodels(x)





















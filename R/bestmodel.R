#' @title Choose the best model
#' @description \code{bestmodel} will find the best models from a \code{quickmodel} object.
#' @param quickmodel A \code{quickmodel} object.
#' @return The best model based on the evaluation metric.
#' @details
#' This function will take the result from quickmodel and pick the best model.
#' It will pick by the lowest RMSE for regression and highest Accuracy for classification.
#' Notes: This function now only considers RMSE and Accuracy as the metric.
#' @examples
#' # classification model
#' data(PIMA)
#' class <- quickmodel(Diabetes~., data = PIMA)
#' bm <- bestmodel(class)
#'
#' # regression model
#' data(Boston)
#' reg <- quickmodel(medv~., data = Boston)
#' bm <- bestmodel(reg)
#' @import caret
#' @import randomForest
#' @import rpart
#' @import gbm
#' @import plyr
#' @import glmnet
#' @import Matrix
#' @rdname bestmodel
#' @export

bestmodel <- function(quickmodel) {
  models <- quickmodel$models
  methods <- quickmodel$methods
  metric_values <- data.frame(method = character(), metric = numeric())
  metricname <- quickmodel$metric
  for (method in methods){
    model <- models[[method]]
    metric <- mean(model$results[[metricname]])
    metric_values <- rbind(data.frame(Model = method, metric = metric), metric_values)
  }

  max_metrics <- c("Accuracy", "ROC")
  min_metrics <- c("RMSE")

  if (metricname %in% max_metrics) {
    best_model_index <- which.max(metric_values$metric)
  } else if (metricname %in% min_metrics) {
    best_model_index <- which.min(metric_values$metric)
  }

  names(metric_values)[2] <- metricname

  bestmodelname <- metric_values$Model[best_model_index]
  cat(paste0("The best model based on ", metricname, " is ", bestmodelname, "\n\n"))
  print(metric_values)

  best_model <- models[[metric_values$Model[best_model_index]]]
}



#' @title Choose the best model
#' @description \code{bestmodels} will find the best models from a \code{quickmodel} object.
#' @param quickmodel A \code{quickmodel} object
#' @return The model with highest accuracy.
#' @details
#' This function will take the result from quickmodel and pick the best model.
#' It will pick by the lowest RMSE for regression and highest Accuracy for classification.
#' Notes: This function now only considers RMSE and Accuracy as the metric.
#' @examples
#' # classification models
#' data("PIMA", package="regclass")
#' x=quickmodel(Diabetes~., data = PIMA)
#' bm=bestmodel(x)
#' # regression models
#' data(Boston, package="MASS")
#' x=quickmodel(medv~., data = Boston)
#' bm=bestmodel(x)
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
    metric_values <- rbind(data.frame(method = method, metric = metric), metric_values)
  }

  max_metrics <- c("Accuracy", "ROC")
  min_metrics <- c("RMSE")

  if (metricname %in% max_metrics) {
    best_model_index <- which.max(metric_values$metric)
  } else if (metricname %in% min_metrics) {
    best_model_index <- which.min(metric_values$metric)
  }

  names(metric_values)[2] <- metricname
  print(metric_values)

  best_model <- models[[metric_values$method[best_model_index]]]
}

best <- bestmodel(quickmodel)


# bestmodel <- function(quickmodel) {
#   models <- quickmodel$models
#   methods <- quickmodel$methods
#   if (quickmodel$metric == "RMSE"){
#     rmse_values <- data.frame(method = character(), RMSE = numeric())
#     for (method in methods){
#       model <- models[[method]]
#       rmse <- mean(model$results$RMSE)
#       rmse_values <- rbind(data.frame(method = method, RMSE = rmse), rmse_values)
#     }
#     print(rmse_values)
#     best_model_index <- which.min(rmse_values$RMSE)
#     best_model <- models[[rmse_values$method[best_model_index]]]
#   } else if(quickmodel$metric == "Accuracy"){
#     accuracy_values <- data.frame(method=character(), Accuracy=numeric())
#     for (method in methods){
#       model <- models[[method]]
#       accuracy <- mean(model$results$Accuracy)
#       accuracy_values <- rbind(data.frame(method = method, Accuracy = accuracy), accuracy_values)
#     }
#     print(accuracy_values)
#     best_model_index <- which.max(accuracy_values$Accuracy)
#     best_model <- models[[accuracy_values$method[best_model_index]]]
#   }
#   return(best_model)
# }

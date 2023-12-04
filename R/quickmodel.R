# quantitative
quant_models <- c("lm", "knn", "rf", "rpart", "gbm", "glmnet", "xgbTree")
quant_models <- c("lm", "rf")

# categorical
categ_models <- c("knn", "rf", "rpart", "gbm", "glmnet", "xgbTree")


# main function
quickmodel <- function(formula,
                       data, # all data
                       metric = "Accuracy",
                       methods = c(),
                       trControl = trainControl(),
                       tuneGrid = NULL,
                       tuneLength = 3,
                       partition = 0.8,
                       ...
                       ) {

  # if not installed then install
  if (!require(caret)) {
    install.packages("caret")
  }
  require("caret")

  # extract y varname
  y <- all.vars(formula)[1]

  # change y var to factor if character
  if (is.character(data[[y]])) {
    data[[y]] <- factor(data[[y]])
  }

  # split data into train/test
  set.seed(1234)
  index <- createDataPartition(data[[y]], p = partition, list = FALSE)
  train <- data[index, ]
  test <- data[-index, ]

  # set evaluation metric
  metric <- ifelse(is.factor(data[[y]]), "Accuracy", "RMSE")

  # specify which models to train
  if (is.factor(data[[y]])) {
    methods <- categ_models
  } else {
    methods <- quant_models
  }

  print(methods)

  # model list
  models <- list()

  # create models
  for (method in methods){
    model <- train(
      formula,
      data = train,
      method = method,
      metric = metric,
      maximize = ifelse(metric == "RMSE", FALSE, TRUE),
      trControl = trControl,
      tuneLength = tuneLength,
      tuneGrid = tuneGrid,
      ...
    )

    models[[method]] <- model
  }

  # result
  result <- list(train = train, test = test, models = models)
  class(result) <- "quickmodel"
  return(result)
}


# Testing -----------------------------------------------------------------

library(ggplot2)
data(mpg)
data(mtcars)
x <- quickmodel(mpg ~ ., data = mtcars)




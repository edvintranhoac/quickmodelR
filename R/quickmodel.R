# quantitative
quant_models <- c("lm", "knn", "rf", "rpart", "gbm", "glmnet")
quant_models <- c("lm", "rf", "rf", "rpart", "glmnet")

# categorical
categ_models <- c("knn", "rf", "rpart", "gbm", "glmnet", "xgbTree")


# main function
quickmodel <- function(formula,
                       data, # all data
                       metric = NULL,
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

  # set evaluation metric if not specified
  if (is.null(metric)) {
    metric <- ifelse(is.factor(data[[y]]), "Accuracy", "RMSE")
  }

  # specify which models to train
  if (is.factor(data[[y]])) {
    methods <- categ_models
  } else {
    methods <- quant_models
  }

  # loading required packages
  if ("gbm" %in% methods) {
    require(gbm)
  }

  if ("xgbTree" %in% methods) {
    require(xgboost)
  }

  print(methods) # temporary

  # model list
  models <- list()

  # train models
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

data(mtcars)
x <- quickmodel(mpg ~ ., data = mtcars)




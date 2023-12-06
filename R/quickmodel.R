# main function
quickmodel <- function(formula,
                       data, # all data
                       metric ,
                       methods ,
                       trControl = trainControl(),
                       tuneGrid = NULL,
                       tuneLength = 3,
                       partition = 0.8,
                       seed=1234,
                       ...
                       ) {

  # quantitative
  quant_models <- c("lm", "knn", "rf", "rpart", "gbm", "glmnet")

  # categorical
  categ_models <- c("glm", "knn", "rf", "rpart", "gbm", "glmnet")


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
  set.seed(seed)
  index <- createDataPartition(data[[y]], p = partition, list = FALSE)
  train <- data[index, ]
  test <- data[-index, ]

  # set evaluation metric if not specified
  if (missing(metric)) {
    metric <- ifelse(is.factor(data[[y]]), "Accuracy", "RMSE")
    print(metric)
  }

  # specify which models to train
  if (missing(methods)){
    if (is.factor(data[[y]])) {
      methods <- categ_models
    } else {
      methods <- quant_models
    }
  }

  # loading required packages
  if ("gbm" %in% methods) {
    require(gbm)
  }

  # model list
  models <- list()

  # train models
  for (method in methods){
    set.seed(seed)
    model <- train(
      formula,
      data = train,
      method = method,
      metric = metric,
      trControl = trControl,
      tuneLength = tuneLength,
      tuneGrid = tuneGrid,
      ...
    )
    print("----------------")
    print(method)
    print(model)
    models[[method]] <- model
  }

  # result
  result <- list(train = train, test = test, models = models)
  class(result) <- "quickmodel"
  return(result)
}


# Testing -----------------------------------------------------------------
data("PIMA", package="regclass")
x=quickmodel(Diabetes~., data = PIMA)
x=quickmodel(Age~., data = PIMA)
data("mtcars")
y=quickmodel(mpg~., mtcars)
# for (i in quant_models){
  # model=train(mpg~.,
  #             mtcars, # all data
  #             metric = ifelse(is.factor(mtcars[["mpg"]]), "Accuracy", "RMSE"),
  #             methods = "rpart",
  #             trControl = trainControl(),
  #             tuneGrid = NULL,
  #             tuneLength = 3
  # )
  # print(model)
# }

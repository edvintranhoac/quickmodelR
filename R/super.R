# quantitative
quant_models=c("lm", "knn", "rf", "rpart", "gbm", "glmnet", "xgbTree")

# categorical
cate_models=c("glm", "knn", "rf", "rpart", "gbm", "glmnet", "xgbTree")


#main function
quickmodel=function(formula,
                    data, # all data
                    partition = 0.8,
                    methods=c(),
                    trControl=trainControl(),
                    tuneLength=3,
                    metric="Accuracy",
                    tuneGrid=NULL,
                    ...
                    ){
  # if not installed then install
  if(!require(caret)){
    install.packages("caret")
  }
  require("caret")

  y=all.vars(formula)[1]

  if (is.factor(data[[as.character(formula[[2]])]])){
    methods=cate_models
    metric="Accuracy"
  }else{
    methods=quant_models
    metric="RMSE"
  }

  # do partitioning
  set.seed(1234)
  index = createDataPartition(data[[y]], p=partition, list=FALSE)
  train = data[index, ]
  test = data[-index, ]

  #model list
  models=list()
  print(methods)
  #create models
  for (method in methods){
    print(method)
    model=train(
      formula,
      data=train,
      method=method,
      metric=metric,
      trControl=trControl,
      tuneLength=tuneLength,
      tuneGrid=tuneGrid,
      ...
    )
    models$method=model
  }

  #result
  result=list(test=test, train=train, models=models)
  class(result)="quickmodel"
  return(result)

  # ifelse(, cate_models, quant_models)
  # ifelse(is.factor(formula[[2]]), "Accuarcy", "RMSE")
}

#test-------
data(mtcars)
x=quickmodel(mpg~., data=mtcars)

formula <- mpg ~ .

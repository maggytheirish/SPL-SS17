
#' HOW TO:
#' If you call train_models you have to specify the following variables:
#' data: Dataset or Fold of Dataset you are using for training the data
#' test: test set you want to create predictions for
#' method: training method you are using (i.e xgb, rf, etc - IMPORTANT: use Carets naming convention)



#STRINGS NEED TO BE USED AS ARGUMENTS for method, df.version
train_models <- function(data, test, method){
  
  #packages loaded
  library(caret)
  library(doParallel)
  
  #ERROR Handling
  methods <- c("xgbTree", "nnet", "lm", "rf")
  
  if(!is.character(method)){
    stop("method must be a string")
  }
  
  if(!is.element(method, methods)){
    stop("specified method is not 'xgboost', 'nnet', 'lm' or 'rf'")
  }
  
  #register cores
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  ## the options for model selection
  model.control<- trainControl(
    method = "cv", # 'cv' for cross validation
    number = 5, # number of folds in cross validation
    allowParallel = TRUE, # Enable parallelization if available
    returnData = TRUE # We will use this to plot partial dependence
  )
  
  if(method=="xgbTree"){
    tuneGrid <- expand.grid(nrounds = c(100,200,400),
                            max_depth = c(6,8,10),
                            eta = c(0.01,0.05),
                            gamma = 0,
                            colsample_bytree = c(0.3,0.5,1),
                            min_child_weight = 1,
                            subsample = c(0.5,0.6,0.8,1))
  } else if(method=="rf"){
    n <- round(ncol(data)/3,0)
    nd <- n-5
    nup <- n+5
    tuneGrid <- expand.grid(mtry = nd:nup,ntree=500)
  } else if(method=="nnet"){
    tuneGrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
  } else if(method=="lm"){
    tuneGrid <- NULL
  }
  
  #start time
  time.start <- Sys.time()
  print(paste0("Model started at: ", time.start))
  
  
  #train model
  default.model <-caret::train(Sales~.,data = data,  
                               method = method,
                               tuneGrid = tuneGrid,
                               metric = "RMSE", 
                               trControl = model.control)
  
  #time spent 
  time.end <- Sys.time()
  dur <- time.end-time.start
  print( dur )

  stopCluster(cl)
  return(default.model)
}

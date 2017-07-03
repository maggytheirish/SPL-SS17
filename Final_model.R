test_predictions <- readRDS("Predictions_test.RDS")

## Split into two DFs, 50% each, based on revenue

idx_ensemble = createDataPartition(y = test_predictions$sales, p = 0.5, list = FALSE)

trainset_ensemble = test_predictions[idx_ensemble,]
testset_ensemble = test_predictions[-idx_ensemble,]


## Train Ensemble

# Clean columns not required

trainset_ensemble$LineID = NULL
testset_ensemble$LineID = NULL


# XGB Model Setup
model.control<- trainControl(
    method = "cv", # 'cv' for cross validation
    number = 5, # number of folds in cross validation
    allowParallel = TRUE, # Enable parallelization if available
    returnData = TRUE # We will use this to plot partial dependence
)



xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                                 max_depth = c(2, 4), 
                                 eta = c(0.01, 0.05, 0.1, 0.15), 
                                 gamma = 0,
                                 colsample_bytree = c(0.8, 1),
                                 min_child_weight = 1,
                                 subsample = 0.8)

# Train Model
xgb.default <- caret::train(revenue~., data = trainset_ensemble,  
                            method = "xgbTree",
                            tuneGrid = xgb.parms.default,
                            metric = "RMSE", 
                            trControl = model.control)

# Predict 
xgb.default.pred <- predict(xgb.default, newdata = testset_ensemble)

# Trivial Prediction
testset_ensemble$trivial_0 = 0
testset_ensemble$trivial_1 = 1


#rsse xgb
rsse(pred = testset_ensemble$xgbTree.default.b2.fold.1, obs = testset_ensemble$revenue)
#rsse linear model
rsse(pred = testset_ensemble$lm.default.b2.fold.1, obs = testset_ensemble$revenue)
#rsse ensemble
rsse(pred = xgb.default.pred, obs = testset_ensemble$revenue)
#rsse trivial
rsse(pred = testset_ensemble$trivial_0, obs = testset_ensemble$revenue)
rsse(pred = testset_ensemble$trivial_1, obs = testset_ensemble$revenue)


summary(trainset_ensemble$revenue)
summary(testset_ensemble$revenue)

sum(trainset_ensemble$revenue)
sum(testset_ensemble$revenue)


## Split into two DFs, 50% each, based on order

# Add order to predictions

test_predictions = test_predictions[-c(441002:441004),]
test_predictions$order = test_16_woe$order

idx_ensemble_order = createDataPartition(y = test_predictions$order, p = 0.5, list = FALSE)

trainset_ensemble_order = test_predictions[idx_ensemble_order,]
testset_ensemble_order = test_predictions[-idx_ensemble_order,]

# Clean columns not required

trainset_ensemble_order$LineID = NULL
testset_ensemble_order$LineID = NULL
trainset_ensemble_order$order = NULL
testset_ensemble_order$order = NULL


# XGB Model Setup
model.control<- trainControl(
    method = "cv", # 'cv' for cross validation
    number = 3, # number of folds in cross validation
    allowParallel = TRUE, # Enable parallelization if available
    returnData = TRUE # We will use this to plot partial dependence
)



xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                                 max_depth = c(2, 4), 
                                 eta = c(0.01, 0.05, 0.1, 0.15), 
                                 gamma = 0,
                                 colsample_bytree = c(0.8, 1),
                                 min_child_weight = 1,
                                 subsample = 0.8)

# Train Model
xgb.default <- caret::train(revenue~., data = trainset_ensemble_order,  
                            method = "xgbTree",
                            tuneGrid = xgb.parms.default,
                            metric = "RMSE", 
                            trControl = model.control)

# Predict 
xgb.default.pred_order <- predict(xgb.default, newdata = testset_ensemble_order)




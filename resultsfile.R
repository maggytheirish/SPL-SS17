# results <- function(model,newdata=test,model_type=NULL)
# {
#    
#     rmse <- sqrt(mean((actual-pred)^2))
#     bias <- mean(Predictions_test$`test_final.v2$Sales`) - mean(pred)
#     var(Predictions_test$`test_final.v2$Sales`,Predictions_test$lr.b1.v2)
#     
#     result <- list(probabilties=pred,predictions=yhat,auc=area,confusion_matrix=conf.mat,finalscore=score)
#     
#     return(result) }
# 
# 
# skewness(test_final.v2$Sales)
# summary(test_final.v2$Sales)
# skewness(pred)
# summary(pred)

num <- res1$open==0
res1[num==T,]<-0

# Pre treatment 

train_final.v2[,c(7,21:28)] <- scale(train_final.v2[,c(7,21:28)])
test_final.v2[,c(7,21:28)] <- scale(test_final.v2[,c(7,21:28)])
newdata_set.v2[,sapply(newdata_set.v2,is.numeric)] <- scale(newdata_set.v2[,sapply(newdata_set.v2,is.numeric)])
newdata_set.v2$Sales <- NA 

# creating file to store predictions

Predictions_test <- as.data.frame(test_final.v2$Sales)
benchmark <- ifelse(test_final.v2$Open==0,0,avgsalesperstore$x)
Predictions_test$benchmark_0 <- benchmark
saveRDS(Predictions_test,"Predictions_test.RDS")

class<- readRDS("class.b1.v1")
Predictions_class <- setNames(as.data.frame(c(1:nrow(class))),"ID")
saveRDS(Predictions_class,"Predictions_class.RDS")

save_prediction <- function(modelname,modelresults,dataset){
    # Loading the datasets
    
    Predictions_class <- readRDS("Predictions_class.RDS")
    Predictions_test <- readRDS("Predictions_test.RDS")
    
    # check if number of observations is equal
    
    if(dataset=="class"){
        if(nrow(Predictions_class)!=length(modelresults)) print("mismatch in number of rows")
        Predictions_class[,modelname] <- modelresults   
    } else if(dataset=="test") {
        if(nrow(Predictions_test)!=length(modelresults)) print("mismatch in number of rows")
        Predictions_test[,modelname] <- modelresults
       } else 
            print("Please specify the dataset name")
    saveRDS(Predictions_class,"Predictions_class.RDS")
    saveRDS(Predictions_test,"Predictions_test.RDS")
}





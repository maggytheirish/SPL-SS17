###################################################################

#This r script contains all the custom functions used in the code 

###################################################################

### Code for loading required packages ###

load_packages  <- function(p){
  
  for(i in seq_along(p)) {
    if(!require(p[i], character.only=TRUE)) {
      install.packages(p[i])}
      library(p[i], character.only=TRUE)
  }

}


### Function to evaluate models, predictions ###

## This function takes in the model and outputs a result vector containg 
##the probabilities, class probabilities, confusion matrix and the score


results <- function(model,newdata=test,model_type=NULL)
{
  if(is.null(model_type)==T){
    pred <- predict(model,newdata=test,type="prob")[,2]
  }else(pred<- predict(model, newdata=test, type="response"))
  yhat <- ifelse(pred>=0.2307,"yes","no")
  conf.mat <- confusionMatrix(yhat,test$return_customer,positive = "yes")
  score <- (conf.mat$table[1,1]*3-(10*conf.mat$table[1,2]))/nrow(test)
  area <-auc((as.numeric(test$return_customer)-1),ifelse(pred>=0.2307,1,0))

  result <- list(probabilties=pred,predictions=yhat,auc=area,confusion_matrix=conf.mat,finalscore=score)
  
  return(result) }

### Loading the datasets ###

load.multiple.files <- function(path,pattern){
  
  all.csv.files <-list()
  list.filenames<-list.files(path,pattern=glob2rx(pattern))
  
  for (i in 1:length(list.filenames))
  {
    all.csv.files[[i]]<-read.csv(paste(path,list.filenames[i],sep = "/"),sep = ",")
  }
  
  names(all.csv.files)<-list.filenames
  
  return(all.csv.files)
}

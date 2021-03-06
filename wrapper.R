#input - dataset for prediction, model 

evaluate <- function(model,data,actual){
  
  type = readline(prompt = "Choose - Classification or Regression")
  
  if(type == "Classification"){
    
    classifier = readline(prompt = "Type in the name of the classifier : (eg. lr, xgb)")
    
    if(classifier=="lr"){
      probabilities <- predict(model,newdata= data)
    }else {probabilities <- predict(model,newdata= data)}
    
    stats <- summary(probabilities)
    print("Descriptive statistics for the prediction probabilities : ")
    print(stats)
      
    threshold <- readline(prompt = "Do you want to use a specific threshold for classification?
                      Type in the numerical threshold value if not type 'no'")
      
    if(threshold=="no"){
        print("You have chosen to use a default threshold")
        print("Classification using 0.5,mean of the probabilities as thresholds")
        
        pred_random <- ifelse(probabilities>=0.5,pos,neg)
        pred_mean <- ifelse(probabilities>=stats[3],pos,neg) 
        
      } else { pred_user <- ifelse(probabilities>=as.numeric(threshold),pos,neg)}
      
      # Display results
      print("Random Classification")
      print(table(pred_random))
      
      print("Threshold - Mean")
      print(table(pred_mean))
      
  }else { if(type == "Regression") {
    
    # Predictions
    pred <- predict(model,data)
    pred <- ifelse(pred<=0,0,pred) ## Removing negative values
    
    cat("\n","The descriptive statistics of the predicted values","\n")
    print(summary(pred))
    cat("\n","The descriptive statistics of the actual values","\n")
    print(summary(actual))
    
    # Metric 
    metric <- readline(prompt = "Which metric would you like to use for error analysis?
                       Choose btw rmse,user. If you want to use a specific metric please
                       add it to the helper function and name it as user.")
    
    if(metric=="rmse"){

      error <- rmse(actual,pred)
      cat(paste0("\n","The prediction error is : ",error))
    }else{ error <- user(actual,pred)}
    
    
    # Error decomposition
    mse <- mean((actual-pred)^2)
    bias <- abs(mean(actual) - mean(pred))
    variance <- var(actual,pred)  
    unexplained <- abs(mse - (bias^2+variance))/mse
    actual.skew <- skewness(actual)
    predicted.skew <- skewness(pred)
    
    # Saving results 
    
    error.matrix <- cbind(error,mse,bias,variance,unexplained,
                          actual.skew,predicted.skew)
    cat("\n","Decomposing the error : ","\n")
    print(error.matrix)
    
    # Visualization
    
    x1 = actual
    y1 = dnorm(actual,mean = mean(actual),sd=sd(actual))
    
    x2 = pred
    y2 = dnorm(pred,mean = mean(pred),sd=sd(pred))
    
    cat("\n","Plotting the distributions of the actual and predicted values - green actual,red predicted")
    
    dist.plot <- plot(x1,y1,col="green", xlim=range(c(x1,x2)),ylim=range(c(y1,y2)),
                      xlab = "Sales",ylab = "Normalized Sales")
    points(x2,y2,col="red")
    legend("topright",legend=c("Actual","Predicted"),fill=c("green","red"))
    
    dist.plot
    
    pred <- as.vector(round(pred))
    return(pred)
    
  }}

}
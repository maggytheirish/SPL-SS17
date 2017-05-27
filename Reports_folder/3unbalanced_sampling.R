### Using over and under sampling to deal with asymmetric costs

# Loading test and train sets 

train <- readRDS("train.RDS")
test <- readRDS("test.RDS")

# Checking the ratio of 1s and 0s in the train and test sets ( should be equal )

table(train$return_customer)
prop.table(table(train$return_customer))

table(test$return_customer)
prop.table(table(test$return_customer))

# Model results - Decision tree

dt <- rpart(return_customer~.,data = train,minsplit=47,cp=0.00046) 
res.dt <- results(dt)

res.dt$finalscore
res.dt$auc

# To deal with imbalanced classification - using ovun.sample fn of the ROSE package

# By specifying the method as both, we use both over and under sampling

install.packages("ROSE")
library("ROSE")

over_sam <- ovun.sample(return_customer~.,data=train,method = "both",p=0.5)$data
table(over_sam$return_customer)

dt.ov <- rpart(return_customer~.,data = over_sam,minsplit=47,cp=0.00046)
res.dt.ov <- results(dt.ov)
acc.ov <- accuracy.meas(test$return_customer,res.dt.ov$probabilties,threshold =0.2307)
roc.curve(test$return_customer,res.dt.ov$probabilties)

### SMOTE using package unbalanced

install.packages("unbalanced")
library("unbalanced")

newd_id <- ubBalance(train[, !(colnames(train) %in% "return_customer")],train$return_customer,type="ubSMOTE",positive = "yes")


train.SM <- cbind(newd_id$X, "return_customer" = newd_id$Y)
train.SM$weight_miss <- as.logical(train.SM$weight_miss)
train.SM$account<- as.logical(train.SM$account)

prop.table(table(train.SM$return_customer))

rf <- randomForest(return_customer~., data = train, ntree=500,mtry=8)
res.rf <- results(rf)
roc.curve(test$return_customer,res.rf$probabilties)

rf.SM <- randomForest(return_customer~., data = train.SM, ntree=500,mtry=8)
res.rf.SM <- results(rf.SM)
roc.curve(test$return_customer,res.rf.SM$probabilties)

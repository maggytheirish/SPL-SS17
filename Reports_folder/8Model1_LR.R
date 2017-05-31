## Model selection ##

## Logistic regression ##

#Logistic regression with all regressors

lr<- glm(return_customer~ .,data = train, family = binomial(link = "logit"))
summary(lr)
lr.res <- results(model=lr,model_type = "regression")
lr.res$finalscore

#Logistic regression with best variables from random forest

lr.select<- glm(frml_rfvars,data = train, family = binomial(link = "logit"))
summary(lr.select)
lr.res.select <- results(model=lr.select,model_type = "regression")
lr.res.select$finalscore

# Logistic regression using variables from rfe

lr.rfe <- glm(frml_rfe,data=train,family=binomial(link = "logit"))
summary(lr.rfe)
lr.rfe$results
lr.rfe.res <- results(lr.rfe)
lr.rfe.res$finalscore


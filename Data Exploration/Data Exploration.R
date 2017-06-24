# Loading necessary packages

library(data.table)
library(zoo)
library(ggplot2)
library(forecast)
library(caret)

# Loading the train and test datasets

train=read.csv("train.csv")
store=read.csv("store.csv")

# Plot the dependent variable

train_sales = subset(train, Sales != 0, drop = TRUE)
hist(aggregate(train_sales$Sales, 
               by = list(train_sales$Store), mean)$x, 100, 
     main = "Mean sales per store when store was not closed")
hist(train$Sales, 100)
train_sales = subset(train, Sales != 0, drop = TRUE)
hist(aggregate(train_sales$Customers, 
               by = list(train_sales$Store), mean)$x, 100, 
     main = "Mean sales per store when store was not closed")
ggplot(train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

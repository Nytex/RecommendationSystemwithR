#Install the Packages
install.packages("randomForest")
install.packages("dplyr")
install.packages("stats")
install.packages("readxl")
install.packages("str_replace")
install.packages("party")
install.packages("Rcpp")
install.packages('e1071', dependencies=TRUE)
install.packages("rpart.plot")

#Load Libraries
library(randomForest)
library(stats)
library(dplyr)
library(readxl)
library(stringr)
library(caret)
library(party)
library(rpart)
library(rpart.plot)
library(reprtree)

#Read the Data
my_data <- read_excel("Consumer_test.xlsx")
str(my_data)

#Convert the Character into Factor
my_data = my_data %>% mutate_if(is.character, as.factor)

#Convert the Double into Factor except Price
my_data$product_id <- factor(my_data$product_id)
my_data$user_id <- factor(my_data$user_id)

#the Data with NA values
my_data <- na.omit(my_data)
my_data <- my_data[!grepl("NA", my_data$category_code),]
my_data <- my_data[!grepl("NA", my_data$brand),]

#Split the Data into 20% Test and 80% Train Data
pd <- sample(2, nrow(my_data), replace = TRUE, prob = c(0.8, 0.2))
train <- my_data[pd==1, ]
test <- my_data[pd==2, ]

##1. Random Forest Model
#Building   Random Forest
RFM <-  randomForest(product_id~brand+price+category_code, data = train, ntree = 60)
plot(RFM)

#Evaluating Model Accuracy with Test Data
pred_rfm = predict(RFM, test)
confusionMatrix(pred_rfm, test$product_id)

#Histogram for Tree Nodes
hist(treesize(RFM), main = "No of Nodes for the Trees", col = "green")

#Variable Importance
varImpPlot(RFM)

#Plotting the Table with Predicted Results
test_rf <- subset(test[c('user_id','category_code','brand','product_id')])
test_rf$pred_rfm <- pred_rfm
test_rf

##2. Decision Tree Model
#Decision Tree with RPART
tree_r <- rpart(product_id~brand+price+category_code, data = train, )
rpart.plot(tree_r,  type = 2, extra =3, tweak = 1.2, faclen = 2)

#Evaluating Model Accuracy with Test Data
pred_tree_r = predict(tree_r, test, type='class')
confusionMatrix(pred_tree_r, test$product_id)

#Plotting the Table with Predicted Results
test_dc <- subset(test[c('user_id','category_code','brand','product_id')])
test_dc$pred_tree_r <- pred_tree_r
test_dc

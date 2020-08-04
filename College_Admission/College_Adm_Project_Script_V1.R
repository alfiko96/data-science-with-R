# Knit functionality in R


##  Import data into R environment.
CollegeAdm <- read.csv(file.choose())

# 1. Find the structure of the data set and if required, transform the numeric data type to factor and vice-versa.

str(CollegeAdm)

data.frame(names(CollegeAdm))

# 1             admit -> Cat [Target or Dependent]
# 2               gre -> Numeric [ Int]
# 3               gpa -> Numeric [Cont]
# 4               ses -> Cat
# 5       Gender_Male -> Cat
# 6              Race -> Cat
# 7              rank -> Cat [Ordinal] 

### b. Convert the required variables into categorical
CollegeAdm$admit <- as.factor(CollegeAdm$admit)
str(CollegeAdm)

## 2. Find the missing values. (if any, perform missing value treatment)

### Check missing values in the entire data 
sum(is.na(CollegeAdm))
### to check missing values for indv columns
sapply(CollegeAdm,FUN = function (x) {sum(is.na(x))})


## 3. Find outliers (if any, then perform outlier treatment)

summary(CollegeAdm)

boxplot(CollegeAdm$gre)

quantile(CollegeAdm$gre,probs = c(0,.01,.05,.25,.5,.75,1))

boxplot(CollegeAdm$gpa)

## 4. Find whether the data is normally distributed or not. Use the plot to determine the same. 
hist(CollegeAdm$gre)
hist(CollegeAdm$gpa)

## 5. Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables) 
### Variables conversion needs to be done first
### Split the data b/w 80:20 [Test]

set.seed(1234)
train_indices <- sample(1:nrow(CollegeAdm), 0.80 * nrow(CollegeAdm))
train <- CollegeAdm[train_indices, ]
test <- CollegeAdm[-train_indices,]

prop.table(table(train$admit))
prop.table(table(test$admit))

log_model<-glm(admit~.,family = "binomial",data = train)
log_model

summary(log_model)

## 6. Use variable reduction techniques to identify significant variables 

### a. multicolenarity

library(car)

vif(log_model)
### b. remove the insignificant vairable where p-value is less than 5%
log_model<-glm(admit~.,family = "binomial",data = CollegeAdm)
summary(log_model)

### For Ex - Remove Gender and build the logistic model once again.
var_rem<-("Gender_Male")
col_loc_rm=which(names(CollegeAdm) %in% var_rem)

log_model <- glm(admit ~ . , family ="binomial",data = CollegeAdm[,-col_loc_rm])
vif(log_model)
summary(log_model)

var_rem<-c("Gender_Male","Race")
col_loc_rm=which(names(CollegeAdm) %in% var_rem)

log_model <- glm(admit ~ . , family ="binomial",data = CollegeAdm[,-col_loc_rm])
vif(log_model)
summary(log_model)

var_rem<-c("Gender_Male","Race","ses")
col_loc_rm=which(names(CollegeAdm) %in% var_rem)

log_model <- glm(admit ~ . , family ="binomial",data = CollegeAdm[,-col_loc_rm])
vif(log_model)
summary(log_model)

## 7. Calculate the accuracy of the model and run validation techniques.

### Need test for validation

pred_train_prob=predict(log_model,CollegeAdm,type="response")
pred_train_class=ifelse(pred_train_prob>=0.5,1,0)

### Confusion Matrix

table(CollegeAdm$admit,pred_train_class)

## 8. Try other modeling techniques like decision tree and SVM and select a champion model 

### Decision Tree 
library(rpart)

mod_tree=rpart(formula = admit~.,data = train,method ="class")

class(mod_tree)

mod_tree

## Prediction on Train Data

pred_tree_train=predict(mod_tree,train,type = "class")

pred_tree_train[1:5]

## Confusion Matrix
table(train$admit,pred_tree_train)

(203+40)/nrow(train)
# 75.9%

## Prediction on test data
pred_tree_test_prob=predict(mod_tree,test,type = "class")

head(pred_tree_test_prob)

pred_tree_test[1:2]

table(test$admit,pred_tree_test_prob)

(52+6)/nrow(test)
#72.5%

mod_tree$variable.importance
#1-rank
#2-gpa
#3-gre
#4-race
#5-ses
#6-gender

# Predicting trainData disease using RandomForest Model

# Loading the random forest library
library(randomForest)


mod_rf=randomForest(admit~.,data = train,ntree=11,importance=T,do.trace=T, type="classification")

# Important variable plot
varImpPlot(mod_rf)

pred_rf_train=predict(mod_rf,train,type = "class")

table(train$admit,pred_rf_train)
(214+94)/nrow(train)
# 0.9625

# Prediction on unseen data-> Test
pred_rf_test=predict(mod_rf,test,type = "class")

# Model accuracy
table(test$admit,pred_rf_test)
(45+8)/nrow(test)
#0.6625

## 9. Determine the accuracy rates for each kind of model 

### compute the accuracy for Rf and DT
## 10. Select the most accurate model (RF)
## Choose the best model out of LR, DT, (RF)
## 11. Identify other Machine learning or statistical techniques
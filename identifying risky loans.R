## Example: Identifying Risky Bank Loans ----

## Step 1: Data collection
credit <- read.csv("credit.csv",header=T)

## Step 2: Exploring and preparing the data ----
head(credit)
dim(credit)
credit[['checking_balance']]=as.factor(credit[['checking_balance']])
credit[['credit_history']]=as.factor(credit[['credit_history']])
credit[['purpose']]=as.factor(credit[['purpose']])
credit[['savings_balance']]=as.factor(credit[['savings_balance']])
credit[['employment_duration']]=as.factor(credit[['employment_duration']])
credit[['other_credit']]=as.factor(credit[['other_credit']])
credit[['housing']]=as.factor(credit[['housing']])
credit[['job']]=as.factor(credit[['job']])
credit[['phone']]=as.factor(credit[['phone']])
credit[['default']]=as.factor(credit[['default']])


# correlation 
# Load the dataset (assuming it's called 'credit')
data("credit")

# Compute the correlation matrix
corr_matrix <- cor(credit[, sapply(credit, is.numeric)])

# Print the correlation matrix
print(corr_matrix)



# plot 


# Install the corrplot package if not installed
install.packages("corrplot")
credit <- read.csv("/Users/sovitnayak/Downloads/credit.csv", header = TRUE)
# Load the required packages
library(corrplot)

# Load the dataset
data("credit")

# Compute the correlation matrix
corr_matrix <- cor(credit[, sapply(credit, is.numeric)])

# Create the correlation plot
corrplot(corr_matrix, method = "circle", type = "upper", order = "hclust", tl.coll = "red")


install.packages("Boruta")
library(Boruta)
install.packages("mlbench")
library(mlbench)
install.packages("caret")
library(caret)
library(randomForest)

str(credit)
set.seed(2024)
bortuta = Boruta(default ~., data = credit, doTrace = 2, maxRuns = 500)
print(bortuta)
plot(bortuta, las = 2, cex.axis = 0.5)
plotImpHistory(bortuta)
#Tentative fix
bor <- TentativeRoughFix(bortuta)
print(bor)
attStats(bortuta)


# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
set.seed(2024)

# split the data frames
credit_train <- credit[1:900, ]
credit_test  <- credit[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))



## Step 3: Training a model on the data ----

# classification tree using rpart
# install.packages("rpart")
library(rpart)
credit_model <- rpart(default ~ ., data = credit_train)
# get basic information about the tree
credit_model

# get more detailed information about the tree
summary(credit_model)

# use the rpart.plot package to create a visualization
# install.packages("rpart.plot")
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(credit_model, digits = 3)



## Step 4: Evaluating model performance ----

# create a factor vector of predictions on test data
credit_pred0 <- predict(credit_model, credit_test)
credit_pred <- ifelse(credit_pred0[,2]>0.5, "Yes", "No")

# cross tabulation of predicted versus actual classes
# install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
hj

accuracy = ((58+14)/(58+14+10+18))*100
print(accuracy)

## Step 5: Improving model performance ----

# use randomForest package for bagging and random forest
# use adabag package for boosting

 install.packages("randomForest")
library(randomForest)

# bagging

set.seed(2024)
bag.model <- randomForest(default ~ ., data = credit_train, mtry = 16, importance = TRUE)
summary(bag.model)
credit_pred_1 <- predict(bag.model, newdata = credit_test)
CrossTable(credit_test$default, credit_pred_1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
bagging_accuracy = 60+14/100*100
print(bagging_accuracy)

# Train the random forest model
set.seed(2024)
rf.model <- randomForest(default ~ ., data = credit_train, ntree = 500, importance = TRUE)

# Plot the variable importance
varImpPlot(rf.model, main = "Variable Importance (Random Forest)")

# random forest


set.seed(2024)
rf.model_2 <- randomForest(default ~ checking_balance + months_loan_duration + credit_history + 
                           amount + savings_balance + employment_duration + percent_of_income + 
                           years_at_residence + age + other_credit + housing, data = credit_train, mtry = 4, ntree = 10000, importance = TRUE)
summary(rf.model_2)
credit_pred_2 <- predict(rf.model, newdata = credit_test)
CrossTable(credit_test$default, credit_pred_2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

random_forest_accuracy = 59+13/100*100
print(random_forest_accuracy)
##############################################################
getNonRejectedFormula(bortuta)
credit_model <- rpart(default ~ ., data = credit_train)



credit_model_2 <- rpart(default ~ checking_balance + months_loan_duration + credit_history + 
                        amount + savings_balance + employment_duration + percent_of_income + 
                        years_at_residence + age + other_credit + housing, data = credit_train)
summary(credit_model_2)

credit_pred0 <- predict(credit_model_2, credit_test)
credit_pred <- ifelse(credit_pred0[,2]>0.5, "Yes", "No")

# cross tabulation of predicted versus actual classes
#install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
decision_tree_accuracy_with_vs = 59+14/100*100
print(decision_tree_accuracy_with_vs)


#########erggtg###geg####

library(rpart)
credit_model_2 <- rpart(default ~ checking_balance + months_loan_duration + credit_history + 
                          amount + savings_balance + employment_duration + percent_of_income + 
                          years_at_residence + age + other_credit + housing, data = credit_train)
# get basic information about the tree
credit_model_2

# get more detailed information about the tree
summary(credit_model_2)

# use the rpart.plot package to create a visualization
# install.packages("rpart.plot")
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(credit_model_2, digits = 2)


library(randomForest)

# bagging

set.seed(2024)
bag.model_2 <- randomForest(default ~ checking_balance + months_loan_duration + credit_history + 
                             amount + savings_balance + employment_duration + percent_of_income + 
                             years_at_residence + age + other_credit + housing, data = credit_train, mtry = 16, importance = TRUE)
summary(bag.model_2)
credit_pred_1 <- predict(bag.model_2, newdata = credit_test)
CrossTable(credit_test$default, credit_pred_1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
bagging_accuracy_with_vs = 59+14/100*100
print(bagging_accuracy_with_vs)
#rf

set.seed(2024)
rf.model_2 <- randomForest(default ~ checking_balance + months_loan_duration + credit_history + 
                             amount + savings_balance + employment_duration + percent_of_income + 
                             years_at_residence + age + other_credit + housing, data = credit_train, mtry = 4, ntree = 10000, importance = TRUE)
summary(rf.model_2)
credit_pred_2 <- predict(rf.model_2, newdata = credit_test)
CrossTable(credit_test$default, credit_pred_2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

random_forest_accuracy_with_vs = 59+15/100*100
print(random_forest_accuracy_with_vs)
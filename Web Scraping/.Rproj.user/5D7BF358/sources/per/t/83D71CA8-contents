install.packages("Metrics")
library(Metrics)

install.packages("caret")
library(caret)

install.packages("rpart")
library(rpart)

install.packages("randomForest")
library(randomForest)

install.packages("e1071")
library(e1071)

A <- c(9:18)
B <- c(10:20)
B
A

mae(A,B)

##############################################################################################################

data(mtcars)
View(mtcars)

#Extracting 70% of the Dataset
sample_size <- floor(0.7 * nrow(mtcars))
train_index <- sample(seq_len(nrow(mtcars)), size = sample_size)

#Assigning the 70% and remaining 30% to train and test
train <- mtcars[train_index, ]
test <- mtcars[-train_index, ]

View(train)
View(test)
##############################

#Predicting mpg, based on wt, hp and cyl using train dataset (Multiple Linear regression)
lm_model <- lm(mpg ~ wt + hp + cyl, data = train)
summary(lm_model)

##########Model prediction using test dataset

lm_predictions <- predict(lm_model, newdata = test)
summary(lm_predictions)

#########################Evaluation- Use caret package to calculate performance metrics

lm_performance <- postResample(pred = lm_predictions, obs = test$mpg)
print(lm_performance)



mae_LR <- mae(test$mpg, lm_predictions)
##########################################################SVR_RBF

svr_rbf <- svm(mpg ~ wt + hp + cyl, data = train)
svr_rbf_predictions <- predict(svr_rbf, newdata = test)

svr_performance1 <- postResample(pred = svr_rbf_predictions, obs = test$mpg)
print(svr_performance1)

mae_SVR_RBF <- mae(test$mpg, svr_rbf_predictions)

##########################################################SVR_LINEAR

svr_linear <- svm(mpg ~ wt + hp + cyl, data = train)
svr_linear_predictions <- predict(svr_linear, newdata = test, kernel= "linear")

svr_performance2 <- postResample(pred = svr_linear_predictions, obs = test$mpg)
print(svr_performance2)

mae_SVR_LINEAR <- mae(test$mpg, svr_linear_predictions)

##########################################################SVR_POLY

svr_poly <- svm(mpg ~ wt + hp + cyl, data = train)
svr_poly_predictions <- predict(svr_poly, newdata = test, kernel= "poly")

svr_performance3 <- postResample(pred = svr_poly_predictions, obs = test$mpg)
print(svr_performance3)

mae_SVR_POLY <- mae(test$mpg, svr_poly_predictions)
###################################################################################### decision Tree

dt_model <- rpart(mpg ~ wt + hp + cyl, data = train)
rf_predictions_n100 <- predict(dt_model, newdata = test)

rf_performance_n100 <- postResample(pred = rf_predictions_n100, obs = test$mpg)
print(rf_predictions_n100)
mae_DT <- mae(test$mpg, rf_predictions_n100)
###################################################################################### Random Forest n100
rf_model_n100 <- randomForest(mpg ~ wt + hp + cyl, data = train)
rf_predictions1_n100 <- predict(rf_model_n100, newdata = test)

rf_performance_n100 <- postResample(pred = rf_predictions1_n100, obs = test$mpg)
print(rf_predictions1_n100)
mae_RF <- mae(test$mpg, rf_predictions1_n100)

###################################################################################### Random Forest n200
rf_model_n200 <- randomForest(mpg ~ wt + hp + cyl, data = train)
rf_predictions2_n200 <- predict(rf_model_n200, newdata = test)

rf_performance_n200 <- postResample(pred = rf_predictions2_n200, obs = test$mpg)
print(rf_predictions2_n200)
mae_RF200 <- mae(test$mpg, rf_predictions2_n200)

###################################################################################### Random Forest n200
rf_model_n500 <- randomForest(mpg ~ wt + hp + cyl, data = train)
rf_predictions3_n500 <- predict(rf_model_n500, newdata = test)

rf_performance_n500 <- postResample(pred = rf_predictions3_n500, obs = test$mpg)
print(rf_predictions3_n500)
mae_RF500 <- mae(test$mpg, rf_predictions3_n500)

mae_LR
mae_SVR_RBF
mae_SVR_LINEAR
mae_SVR_POLY
mae_DT 
mae_RF
mae_RF200
mae_RF500

mae <- c("mae_LR",
        " mae_SVR_RBF",
         "mae_SVR_LINEAR",
         "mae_SVR_POLY",
         "mae_DT", 
         "mae_RF",
         "mae_RF200",
         "mae_RF500")

# Define numeric mae values
mae_values <- c(2.832399, 2.598195, 2.598195, 2.598195, 5.552771, 5.552771, 5.552771, 5.552771)

# Define corresponding model names
mae_labels <- c("mae_LR", "mae_SVR_RBF", "mae_SVR_LINEAR", "mae_SVR_POLY", 
                "mae_DT", "mae_RF", "mae_RF200", "mae_RF500")

# Create the barplot
barplot(mae_values, names.arg = mae_labels, col = "skyblue", las = 2,
        main = "Mean Squared Error Comparison", ylab = "mae")

# Optional: Add value labels on bars
text(x = seq_along(mae_values), y = mae_values, labels = round(mae_values, 2), pos = 3, cex = 0.8)
########################################################################################barchat

g

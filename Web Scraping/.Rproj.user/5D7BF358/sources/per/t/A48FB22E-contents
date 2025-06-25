# Load required libraries
library(e1071)      # For SVM
library(rpart)      # For Decision Tree
library(caret)      # For Model Evaluation
library(Metrics)    # For RMSE Calculation
library(randomForest) # For Random Forest

# Load dataset
data(mtcars)
View(mtcars)

# Extracting 70% of the dataset
sample_size <- floor(0.7 * nrow(mtcars))
train_index <- sample(seq_len(nrow(mtcars)), size = sample_size)

# Assigning 70% to train and 30% to test
train <- mtcars[train_index, ]
test <- mtcars[-train_index, ]

View(train)
View(test)

# Multiple Linear Regression
lm_model <- lm(mpg ~ wt + hp + cyl, data = train)
summary(lm_model)

# Model prediction using test dataset
lm_predictions <- predict(lm_model, newdata = test)
summary(lm_predictions)

# Evaluation using caret
lm_performance <- postResample(pred = lm_predictions, obs = test$mpg)
print(lm_performance)

rmse_LR <- rmse(test$mpg, lm_predictions)

# Support Vector Regression (SVR) with RBF Kernel
svr_rbf <- svm(mpg ~ wt + hp + cyl, data = train, kernel = "radial")
svr_rbf_predictions <- predict(svr_rbf, newdata = test)

svr_performance1 <- postResample(pred = svr_rbf_predictions, obs = test$mpg)
print(svr_performance1)

rmse_SVR_RBF <- rmse(test$mpg, svr_rbf_predictions)

# SVR with Linear Kernel
svr_linear <- svm(mpg ~ wt + hp + cyl, data = train, kernel = "linear")
svr_linear_predictions <- predict(svr_linear, newdata = test)

svr_performance2 <- postResample(pred = svr_linear_predictions, obs = test$mpg)
print(svr_performance2)

rmse_SVR_LINEAR <- rmse(test$mpg, svr_linear_predictions)

# SVR with Polynomial Kernel
svr_poly <- svm(mpg ~ wt + hp + cyl, data = train, kernel = "polynomial")
svr_poly_predictions <- predict(svr_poly, newdata = test)

svr_performance3 <- postResample(pred = svr_poly_predictions, obs = test$mpg)
print(svr_performance3)

rmse_SVR_POLY <- rmse(test$mpg, svr_poly_predictions)

# Decision Tree
dt_model <- rpart(mpg ~ wt + hp + cyl, data = train)
dt_predictions <- predict(dt_model, newdata = test)

dt_performance <- postResample(pred = dt_predictions, obs = test$mpg)
print(dt_performance)

rmse_DT <- rmse(test$mpg, dt_predictions)

# Random Forest with 100 Trees
rf_model_n100 <- randomForest(mpg ~ wt + hp + cyl, data = train, ntree = 100)
rf_predictions_n100 <- predict(rf_model_n100, newdata = test)

rf_performance_n100 <- postResample(pred = rf_predictions_n100, obs = test$mpg)
print(rf_performance_n100)

rmse_RF <- rmse(test$mpg, rf_predictions_n100)

# Random Forest with 200 Trees
rf_model_n200 <- randomForest(mpg ~ wt + hp + cyl, data = train, ntree = 200)
rf_predictions_n200 <- predict(rf_model_n200, newdata = test)

rf_performance_n200 <- postResample(pred = rf_predictions_n200, obs = test$mpg)
print(rf_performance_n200)

rmse_RF200 <- rmse(test$mpg, rf_predictions_n200)

# Random Forest with 500 Trees
rf_model_n500 <- randomForest(mpg ~ wt + hp + cyl, data = train, ntree = 500)
rf_predictions_n500 <- predict(rf_model_n500, newdata = test)

rf_performance_n500 <- postResample(pred = rf_predictions_n500, obs = test$mpg)
print(rf_performance_n500)

rmse_RF500 <- rmse(test$mpg, rf_predictions_n500)

# Print RMSE values
print(rmse_LR)
print(rmse_SVR_RBF)
print(rmse_SVR_LINEAR)
print(rmse_SVR_POLY)
print(rmse_DT)
print(rmse_RF)
print(rmse_RF200)
print(rmse_RF500)


# Store RMSE values in a named vector
rmse_values <- c(
  rmse_LR,
  rmse_SVR_RBF,
  rmse_SVR_LINEAR,
  rmse_SVR_POLY,
  rmse_DT,
  rmse_RF,
  rmse_RF200,
  rmse_RF500
)

# Model names
model_names <- c(
  "LR",
  "SVR_RBF",
  "SVR_LINEAR",
  "SVR_POLY",
  "DT",
  "RF_100",
  "RF_200",
  "RF_500"
)

# Create bar plot
barplot(
  rmse_values,
  names.arg = model_names,
  col = "steelblue",
  main = "RMSE Comparison of Different Models",
  ylab = "RMSE",
  xlab = "Models",
  las = 2 # Rotate x-axis labels for readability
)

# Add values on top of bars
text(
  x = seq_along(rmse_values), 
  y = rmse_values, 
  labels = round(rmse_values, 2), 
  pos = 3, 
  cex = 0.8, 
  col = "red"
)
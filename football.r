# Load required libraries
library(randomForest)
library(glmnet)
library(caret)
library(dplyr)
library(ggplot2)
library(corrplot)

# Step 1: Load Dataset
data <- read.csv(file.choose())

# Step 2: Data Preprocessing
# Convert categorical variables to factors
data$IsHome <- as.factor(data$IsHome)

# Split dataset into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$Target, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Step 3: Exploratory Data Analysis (EDA)
# Correlation plot
num_features <- select_if(data, is.numeric)
corr_matrix <- cor(num_features)
corrplot(corr_matrix, method = "circle")

# Visualize Goals vs ImpactScore
ggplot(data, aes(x = Goals, y = Target)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Goals vs Impact Score", x = "Goals", y = "Impact Score")

# Step 4: Feature Engineering
# Add a feature: Goals per Match
data$GoalsPerMatch <- data$Goals / data$Matches
trainData$GoalsPerMatch <- trainData$Goals / trainData$Matches
testData$GoalsPerMatch <- testData$Goals / testData$Matches

# Step 5: Model Building
# Random Forest
rf_model <- randomForest(Target ~ ., data = trainData, ntree = 500, mtry = 3)
rf_predictions <- predict(rf_model, testData)

# Ridge Regression
x_train <- model.matrix(Target ~ ., trainData)[, -1]
y_train <- trainData$Target
x_test <- model.matrix(Target ~ ., testData)[, -1]

ridge_model <- glmnet(x_train, y_train, alpha = 0)
ridge_predictions <- predict(ridge_model, newx = x_test, s = 0.1)

# Step 6: Model Evaluation
# Random Forest Metrics
rf_rmse <- sqrt(mean((rf_predictions - testData$Target)^2))
cat("Random Forest RMSE:", rf_rmse, "\n")

# Ridge Regression Metrics
ridge_rmse <- sqrt(mean((ridge_predictions - testData$Target)^2))
cat("Ridge Regression RMSE:", ridge_rmse, "\n")

# Step 7: Insights
# Feature Importance from Random Forest
importance <- importance(rf_model)
varImpPlot(rf_model)

# Save the Random Forest Model
saveRDS(rf_model, "player_prediction_rf_model.rds")

# Step 8: Visualizing Predictions
ggplot(data.frame(Actual = testData$Target, Predicted = rf_predictions),
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted (Random Forest)", x = "Actual", y = "Predicted")

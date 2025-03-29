


# Random Forest Classification Algorithm


# Importing the dataset
dataset = read.csv(file.choose())
dataset = dataset[-1]
View(dataset)

# Encoding the target feature as factor
dataset$DiseasePresent = factor(dataset$DiseasePresent, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$DiseasePresent, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
train_scale <- scale(training_set[, 1:6])
train_scale
test_scale <- scale(test_set[, 1:6])
test_scale

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[, 1:6],
                          y = training_set$DiseasePresent,
                          ntree = 500)

y_pred = predict(classifier, newdata = test_set[, 1:6])
y_pred

# Making the Confusion Matrix
cm = table(test_set$DiseasePresent, y_pred)
cm

# Convert the confusion matrix to a data frame for ggplot2
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# Plotting the confusion matrix using ggplot2
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "green", high = "darkgreen") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()

#Accuracy Prediction
Accuracy_RF <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(Accuracy_RF * 100, 2), "%"))


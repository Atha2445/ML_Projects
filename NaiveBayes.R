# Naive Bayes Classification Algorithm

# Importing the dataset
dataset = read.csv(file.choose())
dataset = dataset[-1]
View(dataset)

# Using Normalization technique
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n <- as.data.frame(lapply(dataset[1:6],
                               normalize))

data_n$DiseasePresent <- dataset$DiseasePresent

# Loading package
library(e1071)
library(caTools)

# Splitting data into train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
split
train_cl <- subset(dataset, split == "TRUE")
train_cl
test_cl <- subset(dataset, split == "FALSE")
test_cl

# Feature Scaling
train_scale <- scale(train_cl[, 1:6])
train_scale
test_scale <- scale(test_cl[, 1:6])
test_scale

# Fitting Naive Bayes Model to training dataset

classifier_cl <- naiveBayes(DiseasePresent ~ .,
                            data = train_cl)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl,
                  newdata = test_cl)
y_pred

# Confusion Matrix
cm <- table(test_cl$DiseasePresent, y_pred)
cm

# Convert the confusion matrix to a data frame for ggplot2
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# Plotting the confusion matrix using ggplot2
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "brown", high = "pink") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()

#Accuracy Prediction
Accuracy_NB <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(Accuracy_NB * 100, 2), "%"))



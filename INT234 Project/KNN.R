# CA-3 Assignment - Applying machine learning classification 
# algorithms to find the accuracy of the dataset 

# Crop Disease Detection dataset

#K-Nearest Neighbor Classification

# Importing the dataset
data=read.csv(file.choose())
View(data)
data=data[-1]

# Using Normalization technique
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_n <- as.data.frame(lapply(data[1:6],
                               normalize))

data_n$DiseasePresent <- data$DiseasePresent

# Splitting the dataset into the Training set and Test set
install.packages("CaTools")
library(caTools)
set.seed(42)  
split <- sample.split(data_n$DiseasePresent, SplitRatio = 0.7)
data_train <- subset(data_n, split == TRUE)
data_test <- subset(data_n, split == FALSE)

data_train_labels <- data_train$DiseasePresent
data_test_labels <- data_test$DiseasePresent

data_train <- data_train[-ncol(data_train)]  
data_test <- data_test[-ncol(data_test)]     


# install.packages("class")
library(class)

# KNN Implementation
data_test_pred <- knn(train = data_train,
                      test = data_test,
                      cl = data_train_labels,
                      k = 22)
data_test_pred

install.packages("gmodels")
library(gmodels)
CrossTable(x = data_test_labels,
           y = data_test_pred
)

# Confusion Matrix
cm <- table(data_test_labels, data_test_pred)
cm

# Add ggplot2 visualization for Confusion Matrix
install.packages("ggplot2")
library(ggplot2)

# Convert the confusion matrix to a data frame for ggplot2
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# Plotting the confusion matrix using ggplot2
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()

# Prediction of Accuracy
Accuracy_KNN <- sum(data_test_pred == data_test_labels)/length(data_test_labels)
print(paste("Accuracy:", round(Accuracy_KNN * 100, 2), "%"))




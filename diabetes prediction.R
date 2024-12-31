# Install required packages
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("mlbench")) install.packages("mlbench")

# Load necessary libraries
library(caret)
library(mlbench)

# Load the PimaIndiansDiabetes dataset
data(PimaIndiansDiabetes)

# View a summary of the data
summary(PimaIndiansDiabetes)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
train_index <- createDataPartition(PimaIndiansDiabetes$diabetes, p = 0.75, list = FALSE)
train_data <- PimaIndiansDiabetes[train_index, ]
test_data <- PimaIndiansDiabetes[-train_index, ]

# Train a logistic regression model
model <- train(
  diabetes ~ .,  # Formula: Predict diabetes using all other variables
  data = train_data,
  method = "glm",  # Logistic regression
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
)

# Print the model summary
print(model)

# Make predictions on the test set (probabilities)
pred_prob <- predict(model, newdata = test_data, type = "prob")

# Convert probabilities to binary predictions (threshold = 0.5)
predictions <- ifelse(pred_prob$pos > 0.5, "pos", "neg")

# Evaluate the model's performance
confusion_matrix <- confusionMatrix(as.factor(predictions), test_data$diabetes)
print(confusion_matrix)

# ROC Curve and AUC
if (!require("pROC")) install.packages("pROC")
library(pROC)
roc_curve <- roc(test_data$diabetes, pred_prob$pos)
plot(roc_curve, main = "ROC Curve")
cat("AUC:", auc(roc_curve), "\n")


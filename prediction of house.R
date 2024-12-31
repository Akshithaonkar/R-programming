# Install required packages
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS")

# Load necessary libraries
library(caret)
library(MASS)

# Load the Boston Housing dataset
data("Boston")

# Set a seed for reproducibility
set.seed(123)

# Split the data into training (80%) and testing (20%) sets
train_index <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

# Train a linear regression model
model <- train(
  medv ~ .,  # Formula: Predict median house value (medv) using all other variables
  data = train_data,
  method = "lm",  # Linear regression
  trControl = trainControl(method = "none")  # No cross-validation
)

# Function to take user input and predict house price in INR
predict_house_price_inr <- function() {
  cat("Enter the following values for the house:\n")
  crim <- as.numeric(readline(prompt = "Per capita crime rate (CRIM): "))
  zn <- as.numeric(readline(prompt = "Proportion of residential land zoned for lots over 25,000 sq.ft (ZN): "))
  indus <- as.numeric(readline(prompt = "Proportion of non-retail business acres per town (INDUS): "))
  chas <- as.numeric(readline(prompt = "Charles River adjacency (1=Yes, 0=No) (CHAS): "))
  nox <- as.numeric(readline(prompt = "Nitric oxide concentration (NOX): "))
  rm <- as.numeric(readline(prompt = "Average number of rooms per dwelling (RM): "))
  age <- as.numeric(readline(prompt = "Proportion of owner-occupied units built prior to 1940 (AGE): "))
  dis <- as.numeric(readline(prompt = "Weighted distances to employment centers (DIS): "))
  rad <- as.numeric(readline(prompt = "Index of accessibility to radial highways (RAD): "))
  tax <- as.numeric(readline(prompt = "Full-value property-tax rate per $10,000 (TAX): "))
  ptratio <- as.numeric(readline(prompt = "Pupil-teacher ratio by town (PTRATIO): "))
  black <- as.numeric(readline(prompt = "1000(Bk - 0.63)^2 where Bk is the proportion of Black residents (BLACK): "))
  lstat <- as.numeric(readline(prompt = "Percentage of lower-status population (LSTAT): "))
  
  # Create a new data frame with user input
  new_house <- data.frame(
    crim = crim,
    zn = zn,
    indus = indus,
    chas = chas,
    nox = nox,
    rm = rm,
    age = age,
    dis = dis,
    rad = rad,
    tax = tax,
    ptratio = ptratio,
    black = black,
    lstat = lstat
  )
  
  # Predict house price for the input data in USD
  predicted_price_usd <- predict(model, newdata = new_house)
  
  # Convert predicted price from USD to INR
  usd_to_inr <- 83  # Current exchange rate (you can update it based on the market)
  predicted_price_inr <- predicted_price_usd * 1000 * usd_to_inr  # Multiplying by 1000 as the model predicts in thousands of dollars
  
  cat("\nPredicted Median House Price: ₹", round(predicted_price_inr, 2), "\n")
}

# Test the function
predict_house_price_inr()


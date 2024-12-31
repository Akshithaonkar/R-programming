# Install required packages
if (!require("caret")) install.packages("caret", dependencies = TRUE)

# Load necessary libraries
library(caret)

# Generate a simulated dataset (for the purpose of this example)
set.seed(123)
n <- 500  # Number of employees in the dataset

# Simulate data for employee features
years_at_company <- sample(1:10, n, replace = TRUE)  # Years at the company (1 to 10)
job_satisfaction <- runif(n, 1, 10)  # Job satisfaction (1 to 10)
salary <- sample(30000:120000, n, replace = TRUE)  # Salary (in dollars)
work_life_balance <- runif(n, 1, 10)  # Work-life balance (1 to 10)
promotion_status <- sample(c("Yes", "No"), n, replace = TRUE)  # Recently promoted? (Yes/No)

# Simulate employee turnover (1 for Yes, 0 for No)
turnover <- ifelse(job_satisfaction < 5 & years_at_company < 3, 1, 0)  # Employees with low satisfaction and few years are likely to leave

# Create a data frame
data <- data.frame(
  years_at_company = years_at_company,
  job_satisfaction = job_satisfaction,
  salary = salary,
  work_life_balance = work_life_balance,
  promotion_status = promotion_status,
  turnover = factor(turnover, levels = c(0, 1))  # Factor for binary classification
)

# Convert promotion_status into a factor
data$promotion_status <- factor(data$promotion_status, levels = c("Yes", "No"))

# Split data into training (80%) and testing (20%) sets
train_index <- createDataPartition(data$turnover, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a logistic regression model
model <- train(
  turnover ~ .,  # Formula: Predict turnover based on all other variables
  data = train_data,
  method = "glm",  # Logistic regression for binary classification
  family = "binomial",
  trControl = trainControl(method = "none")  # No cross-validation
)

# Function to take user input and predict employee turnover
predict_turnover <- function() {
  cat("Enter the following details to predict employee turnover:\n")
  years_at_company <- as.numeric(readline(prompt = "Years at Company: "))
  job_satisfaction <- as.numeric(readline(prompt = "Job Satisfaction (1-10): "))
  salary <- as.numeric(readline(prompt = "Salary: "))
  work_life_balance <- as.numeric(readline(prompt = "Work-life Balance (1-10): "))
  promotion_status <- readline(prompt = "Recently Promoted (Yes/No): ")
  
  # Create a new data frame with user input
  new_data <- data.frame(
    years_at_company = years_at_company,
    job_satisfaction = job_satisfaction,
    salary = salary,
    work_life_balance = work_life_balance,
    promotion_status = factor(promotion_status, levels = c("Yes", "No"))
  )
  
  # Predict turnover for the input data
  predicted_turnover <- predict(model, newdata = new_data)
  cat("\nPredicted Employee Turnover: ", ifelse(predicted_turnover == 1, "Yes", "No"), "\n")
}

# Test the function
predict_turnover()

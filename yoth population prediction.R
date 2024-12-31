# Install required packages
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS")

# Load necessary libraries
library(caret)

# Generate a simulated dataset (as an example)
set.seed(123)
n <- 200  # number of data points

# Simulate data for urbanization, education investment, healthcare access, etc.
urbanization <- runif(n, 40, 90)  # Urbanization rate (40% to 90%)
education_investment <- runif(n, 5, 20)  # Government education investment (in percentage)
healthcare_access <- runif(n, 50, 95)  # Youth healthcare access (50% to 95%)
birth_rate <- runif(n, 10, 40)  # Birth rate (10 to 40 per 1000)
migration_rate <- runif(n, -5, 20)  # Migration rate (-5 to 20% youth migration)

# Simulate youth population growth based on these factors
youth_pop <- 0.4 * urbanization + 0.3 * education_investment + 0.2 * healthcare_access + 
  0.1 * birth_rate + 0.05 * migration_rate + rnorm(n, 0, 5)  # Add some noise

# Create a data frame
data <- data.frame(
  urbanization = urbanization,
  education_investment = education_investment,
  healthcare_access = healthcare_access,
  birth_rate = birth_rate,
  migration_rate = migration_rate,
  youth_pop = youth_pop
)

# Split data into training (80%) and testing (20%) sets
train_index <- createDataPartition(data$youth_pop, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a linear regression model
model <- train(
  youth_pop ~ .,  # Formula: Predict youth population using all other variables
  data = train_data,
  method = "lm",  # Linear regression
  trControl = trainControl(method = "none")  # No cross-validation
)

# Function to take user input and predict youth population growth
predict_youth_population <- function() {
  cat("Enter the following details to predict youth population growth:\n")
  urbanization <- as.numeric(readline(prompt = "Urbanization Rate (Percentage of population in urban areas): "))
  education_investment <- as.numeric(readline(prompt = "Education Investment (Percentage): "))
  healthcare_access <- as.numeric(readline(prompt = "Healthcare Access (Percentage of youth with healthcare): "))
  birth_rate <- as.numeric(readline(prompt = "Birth Rate (Births per 1000 people): "))
  migration_rate <- as.numeric(readline(prompt = "Migration Rate (Percentage of youth migrating to the region): "))
  
  # Create a new data frame with user input
  new_data <- data.frame(
    urbanization = urbanization,
    education_investment = education_investment,
    healthcare_access = healthcare_access,
    birth_rate = birth_rate,
    migration_rate = migration_rate
  )
  
  # Predict youth population for the input data
  predicted_youth_pop <- predict(model, newdata = new_data)
  cat("\nPredicted Youth Population Growth: ", round(predicted_youth_pop, 2), "\n")
}

# Test the function
predict_youth_population()


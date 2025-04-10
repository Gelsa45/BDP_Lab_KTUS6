# Linear Regression Analysis Script
# Includes both Simple and Multiple Linear Regression

# Load required libraries
library(ggplot2)
library(lattice)

# ==============================================
# 1. SIMPLE LINEAR REGRESSION (Weight ~ Height)
# ==============================================

simple_linear_regression <- function() {
  cat("\n=== SIMPLE LINEAR REGRESSION ===\n")
  
  # Read data
  data <- read.csv("C:/Users/gelsa/OneDrive/Documents/gelsa/Rpractice/hw.csv")
  cat("\nFirst 6 rows of data:\n")
  print(head(data))
  
  # Fit linear model
  model <- lm(Weight ~ Height, data = data)
  
  # Make predictions
  predictions <- predict(model, newdata = data)
  
  # Calculate evaluation metrics
  mse <- mean((data$Weight - predictions)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(data$Weight - predictions))
  
  # Print metrics
  cat("\nModel Evaluation Metrics:\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  cat("Mean Absolute Error (MAE):", mae, "\n")
  
  # Print model summary
  cat("\nModel Summary:\n")
  print(summary(model))
  
  # Predict for user input
  cat("\nPrediction:\n")
  test_X <- as.numeric(readline("Enter Height value to predict Weight: "))
  prediction <- predict(model, newdata = data.frame(Height = test_X))
  cat("Predicted Weight:", prediction, "\n")
  
  # Visualization
  cat("\nGenerating visualizations...\n")
  
  # ggplot2 visualization
  p1 <- ggplot(data, aes(x = Height, y = Weight)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Simple Linear Regression: Weight ~ Height",
         x = "Height", y = "Weight")
  
  # Lattice visualization
  p2 <- xyplot(Weight ~ Height, data = data,
               type = c("p", "r"),
               main = "Weight vs Height (Lattice)",
               xlab = "Height", ylab = "Weight",
               col = "red", lwd = 2)
  
  # Display plots
  print(p1)
  print(p2)
}

# ==============================================
# 2. MULTIPLE LINEAR REGRESSION (Income ~ Age + Experience)
# ==============================================

multiple_linear_regression <- function() {
  cat("\n=== MULTIPLE LINEAR REGRESSION ===\n")
  
  # Read data
  data <- read.csv("C:/Users/gelsa/OneDrive/Documents/gelsa/Rpractice/income.csv")
  cat("\nFirst 6 rows of data:\n")
  print(head(data))
  
  # Fit linear model
  model <- lm(income ~ age + experience, data = data)
  
  # Make predictions
  predictions <- predict(model, newdata = data)
  
  # Calculate evaluation metrics
  mse <- mean((data$income - predictions)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(data$income - predictions))
  
  # Print metrics
  cat("\nModel Evaluation Metrics:\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  cat("Mean Absolute Error (MAE):", mae, "\n")
  
  # Print model summary
  cat("\nModel Summary:\n")
  print(summary(model))
  
  # Predict for user input
  cat("\nPrediction:\n")
  test_age <- as.numeric(readline("Enter Age value: "))
  test_exp <- as.numeric(readline("Enter Experience value: "))
  new_data <- data.frame(age = test_age, experience = test_exp)
  prediction <- predict(model, newdata = new_data)
  cat("Predicted Income:", prediction, "\n")
  
  # Store predictions and residuals
  data$Predicted_Income <- predict(model)
  data$Residuals <- residuals(model)
  
  # Visualization
  cat("\nGenerating visualizations...\n")
  
  # ggplot2 visualizations
  p1 <- ggplot(data, aes(x = age, y = income)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Age vs Income",
         x = "Age", y = "Income") +
    theme_minimal()
  
  p2 <- ggplot(data, aes(x = experience, y = income)) +
    geom_point(color = "green", alpha = 0.7) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Experience vs Income",
         x = "Experience", y = "Income") +
    theme_minimal()
  
  # Lattice visualizations
  p3 <- xyplot(income ~ age, data = data,
               type = c("p", "r"),
               main = "Age vs Income (Lattice)",
               xlab = "Age", ylab = "Income",
               col = "blue", lwd = 2)
  
  p4 <- xyplot(income ~ experience, data = data,
               type = c("p", "r"),
               main = "Experience vs Income (Lattice)",
               xlab = "Experience", ylab = "Income",
               col = "green", lwd = 2)
  
  # Display plots
  print(p1)
  print(p2)
  print(p3)
  print(p4)
}

# ==============================================
# MAIN PROGRAM EXECUTION
# ==============================================

cat("LINEAR REGRESSION ANALYSIS PROGRAM\n")

while(TRUE) {
  cat("\nChoose analysis type:
      1. Simple Linear Regression (Weight ~ Height)
      2. Multiple Linear Regression (Income ~ Age + Experience)
      3. Exit
      Enter choice (1-3): ")
  
  choice <- as.numeric(readline())
  
  if(choice == 1) {
    simple_linear_regression()
  } else if(choice == 2) {
    multiple_linear_regression()
  } else if(choice == 3) {
    cat("\nExiting program...\n")
    break
  } else {
    cat("\nInvalid choice. Please try again.\n")
  }
}
 # Load required libraries
library(tidyverse)
library(caTools)
library(nnet)
library(caret)

# Function for Binomial Regression (Titanic Dataset)
binomial_regression <- function() {  
  cat("\n--- Loading Titanic Dataset ---\n")
  data <- read.csv("titanic.csv")      # Make sure titanic.csv is in the working directory
  data <- na.omit(data)                # Remove missing values
  
  outcome <- "Survived"  
  predictors <- c("Pclass", "Age", "Sex", "SibSp", "Fare")
  
  set.seed(123)
  split <- sample.split(data[[outcome]], SplitRatio = 0.7)
  train_data <- subset(data, split == TRUE)
  test_data <- subset(data, split == FALSE)
 
  model <- glm(Survived ~ Pclass + Age + Sex + SibSp + Fare, 
               family = binomial, data = train_data)
  
  predictions <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  
  accuracy <- mean(predicted_class == test_data[[outcome]])
  cat("\nModel Accuracy:", round(accuracy * 100, 2), "%\n")
  confusion <- confusionMatrix(as.factor(predicted_class), as.factor(test_data[[outcome]]))
  print(confusion)
  
  # Predict for a new passenger
  cat("\n--- Predicting Survival for a New Passenger ---\n")
  Pclass <- as.integer(readline(prompt = "Enter the passenger class (1, 2, or 3): "))
  Age <- as.numeric(readline(prompt = "Enter the age: "))
  Sex <- readline(prompt = "Enter the sex (male/female): ")
  SibSp <- as.integer(readline(prompt = "Enter number of siblings/spouses aboard: "))
  Fare <- as.numeric(readline(prompt = "Enter the fare: "))
  
  new_data <- data.frame(Pclass = Pclass, Age = Age, Sex = Sex, SibSp = SibSp, Fare = Fare)
  prediction <- predict(model, newdata = new_data, type = "response")
  predicted_class <- ifelse(prediction > 0.5, 1, 0)
  cat("\nPredicted Survival: ", ifelse(predicted_class == 1, "Survived", "Not Survived"), "\n")
}

# Function for Multinomial Regression (Iris Dataset)
multinomial_regression <- function() {
  cat("\n--- Using Built-in Iris Dataset ---\n")
  data <- iris
  
  outcome <- "Species"
  predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  
  set.seed(123)
  split <- sample.split(data[[outcome]], SplitRatio = 0.7)
  train_data <- subset(data, split == TRUE)
  test_data <- subset(data, split == FALSE)
  
  model <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = train_data)
  
  predictions <- predict(model, newdata = test_data)
  
  accuracy <- mean(predictions == test_data[[outcome]])
  cat("\nModel Accuracy:", round(accuracy * 100, 2), "%\n")
  confusion <- confusionMatrix(as.factor(predictions), as.factor(test_data[[outcome]]))
  print(confusion)
  
  # Predict for new flower measurements
  cat("\n--- Predicting Species for New Data ---\n")
  sepal_length <- as.numeric(readline(prompt = "Enter Sepal Length: "))
  sepal_width <- as.numeric(readline(prompt = "Enter Sepal Width: "))
  petal_length <- as.numeric(readline(prompt = "Enter Petal Length: "))
  petal_width <- as.numeric(readline(prompt = "Enter Petal Width: "))
  
  new_data <- data.frame(Sepal.Length = sepal_length, Sepal.Width = sepal_width,
                         Petal.Length = petal_length, Petal.Width = petal_width)
  prediction <- predict(model, newdata = new_data)
  cat("\nPredicted Species:", prediction, "\n")
}

# Menu to choose between Binomial or Multinomial Regression
menu <- function() {  
  repeat {  
    cat("\n===== REGRESSION MENU =====\n")  
    cat("1. Perform Binomial Regression (Titanic Dataset)\n")  
    cat("2. Perform Multinomial Regression (Iris Dataset)\n")  
    cat("3. Exit\n")  
    choice <- as.integer(readline(prompt = "Enter your choice: "))  
    
    if (choice == 1) {  
      binomial_regression()  
    } else if (choice == 2) {  
      multinomial_regression()  
    } else if (choice == 3) {  
      cat("Exiting the program.\n")  
      break  
    } else {  
      cat("Invalid choice. Please try again.\n")  
    }  
  }
}

# Run the menu
menu()

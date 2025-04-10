# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)


# Load the dataset
data <- read.csv("C:/Users/gelsa/OneDrive/Documents/gelsa/Rpractice/tennis.csv")

# Convert categorical variables to factors
data$Outlook <- as.factor(data$Outlook)
data$Temperature <- as.factor(data$Temperature)
data$Humidity <- as.factor(data$Humidity)
data$Wind <- as.factor(data$Wind)
data$PlayTennis <- as.factor(data$PlayTennis)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$PlayTennis, p = 0.8, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Train the ID3 decision tree model
model <- rpart(PlayTennis ~ ., data = trainData, method = "class", minsplit = 2, minbucket = 1)

# Display the decision tree with enhanced visualization
rpart.plot(model, type = 3, extra = 101, fallen.leaves = TRUE, main = "Decision Tree for PlayTennis")


predictions <- predict(model, testData, type = "class")

# Show model accuracy
cat("Confusion Matrix:\n")
print(confusionMatrix(predictions, testData$PlayTennis))

# Function to predict play outcome based on input values
predict_play <- function(outlook, temp, humidity, wind) {
  new_data <- data.frame(Outlook = factor(outlook, levels = levels(data$Outlook)),
                         Temperature = factor(temp, levels = levels(data$Temperature)),
                         Humidity = factor(humidity, levels = levels(data$Humidity)),
                         Wind = factor(wind, levels = levels(data$Wind)))
  prediction <- predict(model, new_data, type = "class")
  return(prediction)
}

# Take inputs from user
outlook <- readline(prompt = "Enter outlook (Sunny, Overcast, Rain): ")
temp <- readline(prompt = "Enter temperature (Hot, Mild, Cool): ")
humidity <- readline(prompt = "Enter humidity (High, Normal): ")
wind <- readline(prompt = "Enter wind (Weak, Strong): ")

# Predict based on user input
user_prediction <- predict_play(outlook, temp, humidity, wind)
cat("\nPrediction → Will play tennis? :", as.character(user_prediction), "\n")

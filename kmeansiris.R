# Function to calculate Euclidean distance
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# Load the iris dataset
data(iris)
iris_data <- iris[, 1:4]  # Use only numerical features

# Scale the data
scaled_data <- scale(iris_data)

# Ask user for number of clusters
k <- as.numeric(readline("Enter number of clusters (k): "))

# Input initial centroids
cat("Enter initial centroids (in scaled units):\n")
centroids <- matrix(NA, nrow = k, ncol = ncol(scaled_data))
for (i in 1:k) {
  for (j in 1:ncol(scaled_data)) {
    centroids[i, j] <- as.numeric(readline(paste("Centroid", i, "-", colnames(scaled_data)[j], ": ")))
  }
}
centroids <- as.data.frame(centroids)

# Initialize cluster assignment vector
cluster_assignment <- rep(0, nrow(scaled_data))
iteration <- 1

repeat {
  # Assign each point to the nearest centroid
  for (i in 1:nrow(scaled_data)) {
    distances <- apply(centroids, 1, function(centroid) euclidean_distance(scaled_data[i, ], centroid))
    cluster_assignment[i] <- which.min(distances)
  }
  
  # Print current cluster assignment
  cat("\nIteration", iteration, " - Cluster Assignments:\n")
  print(cluster_assignment)
  
  # Calculate new centroids
  new_centroids <- centroids
  for (i in 1:k) {
    assigned_points <- scaled_data[cluster_assignment == i, , drop = FALSE]
    if (nrow(assigned_points) > 0) {
      new_centroids[i, ] <- colMeans(assigned_points)
    }
  }
  
  # Check for convergence
  if (all(round(new_centroids, 6) == round(centroids, 6))) {
    cat("\nConverged! Final centroids:\n")
    print(new_centroids)
    cat("\nFinal Cluster Assignments:\n")
    print(cluster_assignment)
    break
  }
  
  # Update for next iteration
  centroids <- new_centroids
  iteration <- iteration + 1
}

# Plotting the result (using first two features)
plot(scaled_data[, 1], scaled_data[, 2],
     col = cluster_assignment, pch = 19,
     xlab = colnames(scaled_data)[1], ylab = colnames(scaled_data)[2],
     main = paste("K-Means Clustering on Iris Data with", k, "clusters"))
points(centroids[, 1], centroids[, 2], col = "black", pch = 4, cex = 2, lwd = 2)


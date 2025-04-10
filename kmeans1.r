library(ggplot2)

# Function to calculate Euclidean distance
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# Load the data
data_points <- read.csv("C:/Users/gelsa/OneDrive/Documents/gelsa/kmeans.csv", header = FALSE)

# Normalize the data
data_points <- as.data.frame(scale(data_points))

# Ask user for number of clusters
k <- as.numeric(readline("Enter number of clusters (k): "))

# Input initial centroids
cat("Enter initial centroids (in scaled units):\n")
centroids <- matrix(NA, nrow = k, ncol = 2)
for (i in 1:k) {
  centroids[i, 1] <- as.numeric(readline(paste("Centroid", i, "- x: ")))
  centroids[i, 2] <- as.numeric(readline(paste("Centroid", i, "- y: ")))
}
centroids <- as.data.frame(centroids)

# Initialize cluster assignment vector
cluster_assignment <- rep(0, nrow(data_points))
iteration <- 1
max_iter <- 100  # maximum number of iterations

repeat {
  # Assign points to nearest centroid
  for (i in 1:nrow(data_points)) {
    distances <- apply(centroids, 1, function(centroid) euclidean_distance(data_points[i, ], centroid))
    cluster_assignment[i] <- which.min(distances)
  }
  
  # Print current cluster assignment
  cat("\nIteration", iteration, " - Cluster Assignments:\n")
  print(cluster_assignment)
  
  # Calculate new centroids
  new_centroids <- centroids
  for (i in 1:k) {
    assigned_points <- data_points[cluster_assignment == i, , drop = FALSE]
    if (nrow(assigned_points) > 0) {
      new_centroids[i, ] <- colMeans(assigned_points)
    }
  }
  
  if (all(new_centroids == centroids)) {
    cat("\nConverged! Final centroids:\n")
    print(new_centroids)
    
    # Combine data with cluster assignments and print
    result <- cbind(data_points, Cluster = cluster_assignment)
    cat("\nFinal cluster assignments with data points:\n")
    print(result)
    
    break
  }
  
  
  # Max iteration check
  if (iteration > max_iter) {
    cat("Reached max iterations. Stopping...\n")
    break
  }
  
  # Update for next iteration
  centroids <- new_centroids
  iteration <- iteration + 1
}


# Base R plot (simple and effective)
plot(data_points[, 1], data_points[, 2],
     col = as.factor(cluster_assignment),
     pch = 19,
     xlab = "X", ylab = "Y",
     main = paste("K-means Clustering with", k, "clusters"))

# Plot centroids as black 'X'
points(centroids[, 1], centroids[, 2],
       col = "black", pch = 4, cex = 2, lwd = 2)

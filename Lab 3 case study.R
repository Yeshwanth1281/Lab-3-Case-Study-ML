# Load required libraries 
library(stats)

# Define the function to simulate student features
simulate_student_features <- function(n = 100) {
  # Set the random seed
  set.seed(260923)
  
  # Generate unique student IDs
  student_ids <- seq(1, n)
  
  # Simulate student engagement
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  
  # Simulate student performance
  student_performance <- rnorm(n, mean = 60, sd = 15)
  
  # Combine the data into a data frame
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  
  # Return the data frame
  return(student_features)
}

# Simulate student data
student_data <- simulate_student_features()

# Perform dimensionality reduction using PCA
pca_result <- prcomp(student_data[, -1], scale. = TRUE)

# Plot scree plot to decide on the number of principal components to keep
plot(1:length(pca_result$sdev), pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Variance Explained", main = "Scree Plot")

# Select the number of principal components based on the scree plot
num_components <- 2

# Extract the scores for the selected number of principal components
pca_scores <- as.data.frame(pca_result$x[, 1:num_components])

# Cluster the data using KMeans
kmeans_clusters <- kmeans(pca_scores, centers = 3, nstart = 25)

# Plot the clusters
plot(pca_scores, col = kmeans_clusters$cluster, main = "Clusters by KMeans", xlab = "PC1", ylab = "PC2")
points(kmeans_clusters$centers[,1:2], col = 1:3, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 8, cex = 1.2)



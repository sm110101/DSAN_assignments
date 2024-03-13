rm(list=ls())
setwd("~/Desktop/Statlearning/Homework/HW2")


# Read Data
B_X = as.matrix(read.table("B_X.txt"))
B_Y = as.matrix(read.table("B_Y.txt"))
C_X = as.matrix(read.table("C_X.txt"))
C_Y = as.matrix(read.table("C_Y.txt"))
D_X = as.matrix(read.table("D_X.txt"))
D_Y = as.matrix(read.table("D_Y.txt"))

# Define a function that calculates euclidian distance of Px1 Vectors to a specified testpoint.
vec_dist = function(a, b) {
  # Error for non-numeric
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("Non-numeric Argument in Function input")
  }
  
  #Error for dimensional mismatch
  if (length(a) != length(b)) {
    stop("Dimensions of Inputs not the same")
  }
  
  sqrt(sum((a - b)^2))
}

#test_pt = rep(0,10)  



# Define nearest neighbors function
knn_pred = function(x_0, X, Y, k) {
  # Ensuring uniformity between target and input data types
  if (!is.numeric(x_0) || !is.numeric(X) || !is.numeric(k)) {
    stop("Non-numeric input in function argument")
  }
  if (!(is.numeric(Y) || is.character(Y))) {
    stop("Invalid target type")
  }
  
  # Dimensionality Check
  if (ncol(X) != length(x_0)) {
    stop("# Columns in prediction matrix != # elements in test vector")
  }
  
  # Calculate Euc Distance
  dist = apply(X, 1, function(row) vec_dist(row, x_0))
  
  # Find the k smallest distances according to the index
  closest_indices = order(dist)[1:k]
  
  # Retrieve target valus of k closest neighbors
  closest_targets = Y[closest_indices]
  
  # Return mean for numeric Y or mode for character Y
  ## assume ties don't exist per class instructions
  if (is.numeric(Y)) {
    return(mean(closest_targets))
  } else if (is.character(Y)) {
    return(names(sort(table(closest_targets), decreasing = TRUE)[1]))
  }
}
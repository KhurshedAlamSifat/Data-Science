heart_disease <- read.csv("C:/Users/Khurshed Alam Sifat/Documents/Data Science/Final Term/heart.csv", header = TRUE, sep = ",")

sum(is.na(heart_disease))
sum(heart_disease=="")
str(heart_disease)
summary(heart_disease)

attributes_to_normalize <- setdiff(names(heart_disease), "target")

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

for (attr in attributes_to_normalize) {
  heart_disease[[attr]] <- min_max_normalize(heart_disease[[attr]])
}

head(heart_disease)

correlation_matrix <- cor(heart_disease)

mask_lower_triangle <- function(mat) {
  mat[lower.tri(mat, diag = TRUE)] <- NA
  mat
}

correlation_matrix <- cor(heart_disease)

upper_triangle <- mask_lower_triangle(correlation_matrix)

sum(heart_disease=="")

library("class")
set.seed(123)

# Split the data into features (X) and the target variable (y)
X <- heart_disease[, attributes_to_normalize]
y <- heart_disease$target

# Define the number of neighbors (k)
k <- 5

# Split the data into training and test sets (70% training, 30% test)
num_samples <- nrow(X)
num_train <- round(0.7 * num_samples)

train_indices <- sample(num_samples, num_train, replace = FALSE)
test_indices <- setdiff(1:num_samples, train_indices)

X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[test_indices, ]
y_test <- y[test_indices]

# Apply k-NN model on the training set
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = k)

# Calculate accuracy
accuracy <- sum(knn_model == y_test) / length(y_test)

# Display the accuracy
cat("Accuracy:", accuracy, "\n")


num_folds <- 10
fold_indices <- cut(seq_along(y), breaks = num_folds, labels = FALSE)

# Initialize a vector to store accuracy scores
accuracy_scores <- numeric(num_folds)

for (fold in 1:num_folds) {
  # Split the data into training and test sets for the current fold
  test_indices <- which(fold_indices == fold)
  train_indices <- setdiff(1:length(y), test_indices)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[test_indices, ]
  y_test <- y[test_indices]
  
  # Apply k-NN model on the training set
  knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = k)
  
  # Calculate accuracy for the current fold
  accuracy_scores[fold] <- sum(knn_model == y_test) / length(y_test)
}

# Calculate the average accuracy across all folds
average_accuracy <- mean(accuracy_scores)

# Display the accuracy scores for each fold and the average accuracy
cat("Accuracy scores for each fold:", accuracy_scores, "\n")
cat("Average Accuracy:", average_accuracy, "\n")
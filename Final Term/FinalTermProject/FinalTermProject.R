heart_disease <- read.csv("C:/Users/Khurshed Alam Sifat/Documents/Data Science/Final Term/heart.csv", header = TRUE, sep = ",")
view(heart_disease)

sum(is.na(heart_disease))
sum(heart_disease=="")
str(heart_disease)
summary(heart_disease)

attributes_to_normalize <- setdiff(names(heart_disease), "target")
min_max_normalize <- function(x){(x - min(x)) / (max(x) - min(x))}
for (attr in attributes_to_normalize){heart_disease[[attr]] <- min_max_normalize(heart_disease[[attr]])}
head(heart_disease)

correlation_matrix <- cor(heart_disease)
heatmap(correlation_matrix)

mask_lower_triangle <- function(mat){mat[lower.tri(mat, diag = TRUE)] <- ""
mat}
upper_triangle <- mask_lower_triangle(correlation_matrix)
sum(upper_triangle>0.05 & upper_triangle<0)

library("caret")
library("class")

set.seed(123)
train_index <- createDataPartition(heart_disease$target, p = 0.7, list = FALSE)
train_data <- heart_disease[train_index, ]
test_data <- heart_disease[-train_index, ]

train_features <- train_data[, -which(names(train_data) == "target")]
train_target <- train_data$target
test_features <- test_data[, -which(names(test_data) == "target")]
test_target <- test_data$target

knn_model <- knn(train_features, test_features, train_target, k = 5)

accuracy <- mean(knn_model == test_target)
cat("Accuracy (Dividing Data into Training and Test Set):", accuracy)


heart_disease$target <- as.factor(heart_disease$target)
control <- trainControl(method = "cv", number = 10)
knn_model_cv <- train(target ~ ., data = heart_disease, method = "knn",
                      trControl = control, preProcess = c("center", "scale"))

accuracy_cv <- knn_model_cv$results$Accuracy
cat("Accuracy (10-Fold Cross-Validation):", mean(accuracy_cv), "\n")

knn_predictions <- predict(knn_model_cv, newdata = heart_disease)
conf_matrix <- confusionMatrix(knn_predictions, heart_disease$target)

true_positive <- conf_matrix$table[1, 1]
false_positive <- conf_matrix$table[2, 1]
true_negative <- conf_matrix$table[1, 2]
false_negative <- conf_matrix$table[2, 2]

recall <- true_positive / (true_positive + false_negative)
precision <- true_positive / (true_positive + false_positive)

print(conf_matrix)
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
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
  mat[lower.tri(mat, diag = TRUE)] <- ""
  mat
}

upper_triangle <- mask_lower_triangle(correlation_matrix)

sum(upper_triangle>0.4 & upper_triangle<1)
upper_triangle>1 & upper_triangle<0.8

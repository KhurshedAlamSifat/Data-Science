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

heart_disease <- read.csv("C:/Users/Khurshed Alam Sifat/Documents/Data Science/Final Term/heart.csv", header = TRUE, sep = ",")

sum(is.na(heart_disease))
sum(heart_disease=="")
str(heart_disease)
summary(heart_disease)
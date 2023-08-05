titanicdata <- read.csv("C:/Users/Khurshed Alam Sifat/Documents/Data Science/Mid Term/Dataset_midterm_Section(B).csv",header=TRUE, sep=",")
View(titanicdata)

names(titanicdata)
str(titanicdata)
dim(titanicdata)
summary(titanicdata)

colSums(is.na(titanicdata))

frequent_gender <- names(which.max(table(titanicdata$Gender)))
frequent_gender
titanicdata$Gender[is.na(titanicdata$Gender)] <- frequent_gender
colSums(is.na(titanicdata))

missing_gender <- is.na(titanicdata$Gender)
titanicdata_1 <- subset(titanicdata, !missing_gender)
colSums(is.na(titanicdata_1))

sum(titanicdata_1$class=="")
titanicdata_1[titanicdata_1$class=="",]

which(titanicdata_1$class=="")

frequent_age <- names(which.max(table(titanicdata$age)))
titanicdata$age[is.na(titanicdata$age)] <- frequent_age
colSums(is.na(titanicdata))

average_age <- round(mean(titanicdata_1$age, na.rm = TRUE))
titanicdata_1$age[is.na(titanicdata_1$age)] <- average_age
titanicdata_1$age <- as.integer(titanicdata_1$age)
colSums(is.na(titanicdata_1))

frequent_class <- names(which.max(table(titanicdata$class[titanicdata$class!=""])))
titanicdata$class[titanicdata$class==""] <- frequent_class
colSums(is.na(titanicdata))

colSums(titanicdata_1 == '')
titanicdata_2 <- titanicdata_1[!titanicdata_1$class == "", ]
colSums(titanicdata_2 == '')

hist(titanicdata_2$age, main = "Histogram of Age" ,col=c(4))
boxplot(titanicdata_2$age, main = "Box plot of Age" ,col=c(4))
boxplot.stats(titanicdata_2$age)$out
plot(titanicdata_2$age, main = "plot of Age" ,col=c(2))

Age_Q1 <- quantile(titanicdata_2$age, 0.25, na.rm = TRUE)
Age_Q3 <- quantile(titanicdata_2$age, 0.75, na.rm = TRUE)
Age_IQR <- Age_Q3 - Age_Q1
lower_age <- Age_Q1 - 1.5 * Age_IQR
upper_age <- Age_Q3 + 1.5 * Age_IQR
age_outliers <- titanicdata_2$age < lower_age | titanicdata_2$age > upper_age
titanicdata_3 <- titanicdata_2[!age_outliers, ]
hist(titanicdata_3$age, main = "Histogram of Age" ,col=c(4))

attribute_names <- names(titanicdata_3)
for (attribute in attribute_names) {
  unique_values <- unique(titanicdata_3[[attribute]])
  print(paste("Unique values in", attribute, ":"))
  print(unique_values)
}

unique(titanicdata_3$who)
titanicdata_3$who <- ifelse(titanicdata_3$who == "mannn","man", titanicdata_3$who)
titanicdata_3$who <- ifelse(titanicdata_3$who == "womann","woman", titanicdata_3$who)
titanicdata_3$who <- ifelse(titanicdata_3$who == "womannn","woman", titanicdata_3$who)
unique(titanicdata_3$who)

summary(titanicdata_3$fare)
boxplot(titanicdata_3$fare, main = "Box Plot of Fare")

Fare_Q1 <- quantile(titanicdata_3$fare, 0.25, na.rm = TRUE)
Fare_Q3 <- quantile(titanicdata_3$fare, 0.75, na.rm = TRUE)
Fare_IQR <- Fare_Q3 - Fare_Q1
lower_Fare <- Fare_Q1 - 1.5 * Fare_IQR
upper_Fare <- Fare_Q3 + 1.5 * Fare_IQR
Fare_noisyvalue <- titanicdata_3$fare < lower_age | titanicdata_3$fare > upper_age
titanicdata_4 <- titanicdata_3[!Fare_noisyvalue, ]
boxplot(titanicdata_4$age, main = "Box Plot of Fare" ,col=c(4))

sum(duplicated(titanicdata_4))
titanicdata_5 <- titanicdata_4[!duplicated(titanicdata_4), ]
sum(duplicated(titanicdata_5))

titanicdata_5$Gender <- factor(titanicdata_5$Gender,levels=c(0,1),labels=c("Male","Female"))
titanicdata_5$fare <- as.numeric(format(round(titanicdata_5$fare,0)))
titanicdata_5$embarked <- factor(titanicdata_5$embarked,levels=c('C','Q','S'),
                                 labels=c("Cherbourg","Queenstown","Southampton"))
titanicdata_5$alone <- factor(titanicdata_5$alone,levels=c(TRUE,FALSE),labels=c("Yes","No"))
titanicdata_5$survived <- factor(titanicdata_5$survived,levels=c(1,0),labels=c("Yes","No"))
head(titanicdata_5)

mean(titanicdata_5$age)
median(titanicdata_5$age)
mode_age <- names(sort(-table(titanicdata_5$age)))[1]
mode_age
var(titanicdata_5$age)
sd(titanicdata_5$age)
library(ggplot2)
ggplot(titanicdata_5, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Distribution of Age")

mean(titanicdata_5$fare)
median(titanicdata_5$fare)
mode_fare <- names(sort(-table(titanicdata_5$fare)))[1]
mode_fare
var(titanicdata_5$fare)
sd(titanicdata_5$fare)
ggplot(titanicdata_5, aes(x = fare)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Fare", y = "Frequency", title = "Distribution of Fare")


hist(titanicdata_5$age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency")
boxplot(titanicdata_5$age, main = "Boxplot of Age")
density(titanicdata_5$age, main = "Density Plot of Age", xlab = "Age")
age_groups <- cut(titanicdata_5$age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80))
barplot(table(age_groups), main = "Bar Plot of Age Groups", xlab = "Age Group", ylab = "Count")

summary(titanicdata_5$age)
summary(titanicdata_5$fare)

titanicdata
dim(titanicdata_5)

titanicdata_6 <- subset(titanicdata_5, select=-who)

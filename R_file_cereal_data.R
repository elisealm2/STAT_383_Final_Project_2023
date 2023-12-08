#Statistics Group - STAT383 Project

setwd("/Users/elise/Documents/Fall 2023/STAT383/project")
cat("\f") # clean the console
rm(list = ls()) # clean the environment
library(readxl) #read the data
library(ggplot2) #for histogram
cereal_data <- read_excel("cereal_data.xlsx", sheet = "edited data")  #locate the dataset
View(cereal_data) #see dataset in new tab

# -----------scatter plots ----------------
#good correlation
plot(cereal_data$protein, cereal_data$vitamins,
     main = "Scatterplot of Protein vs Vitamins",
     xlab = "protein (g)",
     ylab = "vitamins",
     pch=19,
     col="blue",
     abline(lm(vitamins ~ protein,data = cereal_data), col = "red"))

plot(cereal_data$fat, cereal_data$sodium,
     main = "Scatterplot of Fat vs Sodium",
     xlab = "fat (g)",
     ylab = "sodium (g)",
     pch=19,
     col="blue",
     abline(lm(sodium ~ fat,data = cereal_data), col = "red"))

plot(cereal_data$vitamins, cereal_data$fiber,
     main = "Scatterplot of Vitamins vs Fiber",
     xlab = "vitamins",
     ylab = "fiber (g)",
     pch=19,
     col="blue",
     abline(lm(fiber ~ vitamins,data = cereal_data), col = "red"))

plot(cereal_data$sugars, cereal_data$fat,
     main = "Scatterplot of Sugar vs Fat",
     xlab = "sugar (g)",
     ylab = "fat (g)",
     pch=19,
     col="blue",
     abline(lm(fat ~ sugars,data = cereal_data), col = "red"))

plot(cereal_data$protein, cereal_data$fiber,
     main = "Scatterplot of Protein vs Fiber",
     xlab = "protein (g)",
     ylab = "fiber (g)",
     pch=19,
     col="blue",
     abline(lm(fiber ~ protein,data = cereal_data), col = "red"))

#no correlation
plot(cereal_data$protein, cereal_data$sugars,
     main = "Scatterplot of protein vs sugars",
     xlab = "protein (g)",
     ylab = "sugars (g)",
     pch=19,
     col="blue",
     abline(lm(sugars ~ protein,data = cereal_data), col = "red"))

plot(cereal_data$sodium, cereal_data$sugars,
     main = "Scatterplot of Sodium vs Sugar",
     xlab = "sodium (g)",
     ylab = "sugar (g)",
     pch=19,
     col="blue",
     abline(lm(sugars ~ sodium,data = cereal_data), col = "red"))

# ------end scatter plots---------

# ------linear regression---------

#healthy with healthy
fit_lm1 <- lm(protein ~ fiber,data = cereal_data)

fit_lm2 <- lm(protein ~ vitamins,data = cereal_data)

fit_lm3 <- lm(vitamins ~ fiber,data = cereal_data)

#unhealthy with unhealthy
fit_lm4 <- lm(sugars ~ fat,data = cereal_data)

fit_lm5<- lm(sugars ~ sodium,data = cereal_data)

fit_lm6 <- lm(sodium ~ fat,data = cereal_data)

# print summary of the linear regression models
print(summary(fit_lm1))
print(summary(fit_lm2))
print(summary(fit_lm3))
print(summary(fit_lm4))
print(summary(fit_lm5))
print(summary(fit_lm6))

# -------- multiple linear regression ------------------------------------------
#mlr unhealthy with healthy
fit_mlr_1 <- lm( sugars~  protein + vitamins + fiber, data = cereal_data) 

fit_mlr_2 <- lm( fat~  protein + vitamins + fiber, data = cereal_data)

fit_mlr_3 <- lm( sodium~  protein + vitamins + fiber, data = cereal_data)

#mlr healthy with unhealthy
fit_mlr_4 <- lm( protein~  sugars + fat + sodium, data = cereal_data)

fit_mlr_5 <- lm( vitamins~  sugars + fat + sodium, data = cereal_data)

fit_mlr_6 <- lm( fiber~  sugars + fat + sodium, data = cereal_data)

#protein with fat and sugar as sodium 
fit_mlr_7 <- lm( protein~  sugars + fat, data = cereal_data)

# Print the summary of the regression model
print(summary(fit_mlr_1))
print(summary(fit_mlr_2))
print(summary(fit_mlr_3))
print(summary(fit_mlr_4))
print(summary(fit_mlr_5))
print(summary(fit_mlr_6))
print(summary(fit_mlr_7))

# ------end multiple linear regression ------

#------box plots ----

boxplot(protein~mfr,data=cereal_data, main="Box Plot",
        xlab="mfr", ylab="protein")

boxplot(sugars~mfr,data=cereal_data, main="Box Plot",
        xlab="mfr", ylab="sugars")


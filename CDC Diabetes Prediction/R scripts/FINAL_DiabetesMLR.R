rm(list = ls())
gc()
# Load required libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(Amelia)
library(corrplot)
library(pROC)
library(car)

# Set wd
setwd("C:/Users/users/Downloads")

# Read in the dataset
dat = read.csv('diabetes_binary_health_indicators_BRFSS2015.csv', 
               header = TRUE, 
               stringsAsFactors = FALSE, 
               na.strings = '')

# Check dimensions and structure of the dataset
dim(dat)
str(dat)

# Create missing value matrices
matrix.na <- is.na(dat)
pmiss <- colMeans(matrix.na)
nmiss <- rowMeans(matrix.na)

# Visualize missing data with color
missmap(dat, col = c("yellow", "orange"), legend = TRUE)

# Define the actual labels for age groups 1 to 13
age_labels <- c("18-24", "25-29", "30-34", "35-39", "40-44", 
                "45-49", "50-54", "55-59", "60-64", "65-69", 
                "70-74", "75-79", "80+")

# Convert Age to a factor with readable labels
dat$AgeGroup <- factor(dat$Age, levels = 1:13, labels = age_labels)

# Optional: check if it worked
table(dat$AgeGroup)
nrow(dat)

#Filter Outliers (example: BMI < 60)
dat2 <- dat[dat$BMI < 60, ]

#Visual Check of Continuous Variables (Colorful)
cont_vars <- c("BMI", "Age", "PhysHlth", "GenHlth", "MentHlth")

# 2 plots per row
par(mfrow = c(1, 2))

# Use color for histograms and boxplots
for (v in cont_vars) {
  hist(dat2[[v]], 
       main = paste("Histogram of", v), 
       xlab = v, 
       col = "skyblue", 
       border = "white")
  
  boxplot(dat2[[v]], 
          main = paste("Boxplot of", v), 
          xlab = v, 
          col = "lightgreen", 
          border = "darkgreen")
}

#Frequency Table for Categorical Variables
table(dat2$GenHlth)

cor_mat <- cor(dat2[sapply(dat2, is.numeric)])

# Colorful correlation matrix
corrplot(cor_mat, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.cex = 0.9, 
         number.cex = 0.6, 
         addCoef.col = "black")

set.seed(123)
n.train <- floor(nrow(dat2) * 0.6)
id.train <- sample(1:nrow(dat2), n.train)
id.test <- setdiff(1:nrow(dat2), id.train)

# Fit null and full logistic models
obj.null <- glm(Diabetes_binary ~ 1, data = dat2[id.train, ], family = binomial)
obj.full <- glm(Diabetes_binary ~ ., data = dat2[id.train, ], family = binomial)

# Forward Selection
obj.forward <- step(obj.null, scope = list(lower = obj.null, upper = obj.full), direction = "forward", trace = 0)
summary(obj.forward)

# Backward Elimination
obj.backward <- step(obj.full, direction = "backward", trace = 0)
summary(obj.backward)

# Stepwise Selection
obj.stepwise <- step(obj.null, scope = list(lower = obj.null, upper = obj.full), direction = "both", trace = 0)
summary(obj.stepwise)

# Cut-offs
evaluate_model <- function(model, data, actual, cutoffs = c(0.5, 0.3, 0.1)) {
  probs <- predict(model, newdata = data, type = "response")
  results <- list()
  
  for (cutoff in cutoffs) {
    pred <- ifelse(probs > cutoff, 1, 0)
    conf <- table(Predicted = pred, Actual = actual)
    TP <- conf["1", "1"]
    TN <- conf["0", "0"]
    FP <- conf["1", "0"]
    FN <- conf["0", "1"]
    
    accuracy <- mean(pred == actual)
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    results[[as.character(cutoff)]] <- c(
      Accuracy = round(accuracy, 4),
      Sensitivity = round(sensitivity, 4),
      Specificity = round(specificity, 4)
    )
  }
  return(results)
}

# Evaluate models
actual <- as.numeric(as.character(dat2$Diabetes_binary[id.test]))
forward_results <- evaluate_model(obj.forward, dat2[id.test, ], actual)
backward_results <- evaluate_model(obj.backward, dat2[id.test, ], actual)
stepwise_results <- evaluate_model(obj.stepwise, dat2[id.test, ], actual)

# Display metrics at each cutoff
print("Cutoff = 0.5")
print(rbind(Forward = forward_results[["0.5"]],
            Backward = backward_results[["0.5"]],
            Stepwise = stepwise_results[["0.5"]]))

print("Cutoff = 0.3")
print(rbind(Forward = forward_results[["0.3"]],
            Backward = backward_results[["0.3"]],
            Stepwise = stepwise_results[["0.3"]]))

print("Cutoff = 0.1")
print(rbind(Forward = forward_results[["0.1"]],
            Backward = backward_results[["0.1"]],
            Stepwise = stepwise_results[["0.1"]]))

# Stepwise model summary for OR, CI, and p-values
summary(obj.stepwise)

# Odds Ratios and Confidence Intervals
exp_coef <- exp(coef(obj.stepwise))
conf_int <- exp(confint(obj.stepwise))
results <- data.frame(
  OR = round(exp_coef, 3),
  CI_Lower = round(conf_int[, 1], 3),
  CI_Upper = round(conf_int[, 2], 3),
  P_value = coef(summary(obj.stepwise))[, 4]
)
print("Odds Ratios, Confidence Intervals and P-values (Stepwise Model)")
print(results)

# Get predicted probabilities for each model
probs_forward <- predict(obj.forward, newdata = dat2[id.test, ], type = "response")
probs_backward <- predict(obj.backward, newdata = dat2[id.test, ], type = "response")
probs_stepwise <- predict(obj.stepwise, newdata = dat2[id.test, ], type = "response")


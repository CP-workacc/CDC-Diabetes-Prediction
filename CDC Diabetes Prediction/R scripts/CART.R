# ---------------------------
# CART MODEL (75/25 split version)
# ---------------------------

# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)

# Load data
data <- read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\Projects\\CDC Diabetes\\archive (3)\\cleaned_diabetes_data.csv")
data$Diabetes_binary <- as.factor(data$Diabetes_binary)

# Train/test split (75% train, 25% test)
set.seed(123)
index <- createDataPartition(data$Diabetes_binary, p = 0.75, list = FALSE)
train <- data[index, ]
test <- data[-index, ]

# Train initial CART model
cart_model <- rpart(Diabetes_binary ~ ., data = train, method = "class", 
                    control = rpart.control(cp = 0.001, minsplit = 10))

# Find the best cp for pruning
printcp(cart_model)  # See the cp table
best_cp <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "CP"]

# Prune the tree
pruned_cart_model <- prune(cart_model, cp = best_cp)

# Function to evaluate model at different cutoffs
evaluate_cart_model <- function(model, data, cutoffs) {
  pred_prob <- predict(model, newdata = data, type = "prob")[,2]
  results <- list()
  
  for (cutoff in cutoffs) {
    pred_class <- ifelse(pred_prob > cutoff, "1", "0")
    pred_class <- as.factor(pred_class)
    
    conf_matrix <- confusionMatrix(pred_class, data$Diabetes_binary, positive = "1")
    
    metrics <- list(
      Accuracy = round(conf_matrix$overall['Accuracy'], 4),
      Sensitivity = round(conf_matrix$byClass['Sensitivity'], 4),
      Specificity = round(conf_matrix$byClass['Specificity'], 4)
    )
    
    results[[paste0("Cutoff_", cutoff)]] <- metrics
  }
  
  return(results)
}

# Define cutoffs
cutoffs <- c(0.5, 0.3, 0.1)

# Evaluate Minimum Error Tree
cat("---- Minimum Error Tree ----\n")
min_error_results <- evaluate_cart_model(cart_model, test, cutoffs)
print(min_error_results)

# Evaluate Best Pruned Tree
cat("---- Best Pruned Tree ----\n")
best_pruned_results <- evaluate_cart_model(pruned_cart_model, test, cutoffs)
print(best_pruned_results)

# Plot trees
rpart.plot(cart_model, main = "Minimum Error Tree")
rpart.plot(pruned_cart_model, main = "Best Pruned Tree")

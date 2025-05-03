#kNN
#All Variables

rm(list=ls()); gc()
setwd('/Users/truct/OneDrive/Desktop/ISDS 574/Team Project/archive')
dat = read.csv('diabetes_binary_health_indicators_BRFSS2015.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = '')

dat$Diabetes_binary = as.numeric(dat$Diabetes_binary)
dat$Diabetes_binary = dat$Diabetes_binary # healthy -> 0; diabetic/pre-diabetic -> 1
dat[, 2:ncol(dat)] = scale(dat[, 2:ncol(dat)])

#2. Splitting Data into Training and Testing Sets
set.seed(1)
n.train = floor( nrow(dat)*0.75 ) #Determines the training set size as 75% of the total dataset.
ind.train = sample(1:nrow(dat), n.train) #Randomly selects training indices and assigns the rest as test indices.
ind.test = setdiff(1:nrow(dat), ind.train)

require(class) #running kNN for Classification
vars = c("HighBP", "HighChol", "BMI", "HeartDiseaseorAttack", 
         "PhysActivity", "GenHlth", "Age", "Education", "Income","MentHlth","PhysHlth", "HvyAlcoholConsump",
         "Veggies","Sex","CholCheck","AnyHealthcare","Stroke","DiffWalk","Smoker","NoDocbcCost","Fruits")
Xtrain = scale(dat[ind.train, vars])
Xtest = scale(dat[ind.test, vars])
ytrain = as.factor(dat[ind.train, 1])
ypred = knn(Xtrain, Xtest, ytrain, k=3, prob=T) #k=3 neighbors, Uses probability (prob=T) to get estimated class probabilities.
ytest = dat[ind.test,1] #Extracts true labels for the test set and compares them with predictions using a confusion matrix.

get.prob = function(x) {
  prob = attr(x, 'prob')
  ind = which(x == 0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser() Defines a function to determine the best k from a range (k.grid). Uses ct (cutoff threshold) of 0.5 for classification.
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  #Runs kNN for each k and calculates classification errors.
  #Uses a temporary function (fun.tmp) to apply kNN over all values in k.grid.
  
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  #Computes classification error for each k.
  #Stores the errors in a named vector.
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  
  ## it will return a list so I need to unlist it to make it to be a vector
  ##Finds the best k (with the lowest error). Returns a list with:
  #k.optimal → best value of k
  #error.min → minimum classification error
  #error.all → all errors for different k values.
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 9, 2), .1)
obj1

## rerun with the best k
ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred1)

#calculate each
sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}
#show them out
sen(ytest, ypred1)
spe(ytest, ypred1)
fpr(ytest, ypred1)
fnr(ytest, ypred1)

#8. Overall Performance Summary
performance = function(ytest, ypred) {
  measures = c(mean(ytest == ypred),
               sen(ytest, ypred),
               spe(ytest, ypred),
               fpr(ytest, ypred),
               fnr(ytest, ypred))
  names(measures) = c('Accuracy', 'Sensitivity', 'Specificity', 'FPR', 'FNR')
  return(measures)
}

performance(ytest, ypred1)

#Screened Variables
rm(list=ls()); gc()
setwd('/Users/truct/OneDrive/Desktop/ISDS 574/Team Project/archive')
dat = read.csv('diabetes_binary_health_indicators_BRFSS2015.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = '')

dat$Diabetes_binary = as.numeric(dat$Diabetes_binary)
dat$Diabetes_binary = dat$Diabetes_binary # healthy -> 0; diabetic/pre-diabetic -> 1
dat[, 2:ncol(dat)] = scale(dat[, 2:ncol(dat)])

#2. Splitting Data into Training and Testing Sets
set.seed(1)
n.train = floor( nrow(dat)*0.75 ) #Determines the training set size as 75% of the total dataset.
ind.train = sample(1:nrow(dat), n.train) #Randomly selects training indices and assigns the rest as test indices.
ind.test = setdiff(1:nrow(dat), ind.train)

require(class) #running kNN for Classification
vars = c("GenHlth","HighBP","BMI","DiffWalk","HighChol","Age","HeartDiseaseorAttack", 
         "PhysHlth")
Xtrain = scale(dat[ind.train, vars])
Xtest = scale(dat[ind.test, vars])
ytrain = as.factor(dat[ind.train, 1])
ypred = knn(Xtrain, Xtest, ytrain, k=3, prob=T) #k=3 neighbors, Uses probability (prob=T) to get estimated class probabilities.
ytest = dat[ind.test,1] #Extracts true labels for the test set and compares them with predictions using a confusion matrix.

get.prob = function(x) {
  prob = attr(x, 'prob')
  ind = which(x == 0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .1) {
  # browser() Defines a function to determine the best k from a range (k.grid). Uses ct (cutoff threshold) of 0.5 for classification.
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  #Runs kNN for each k and calculates classification errors.
  #Uses a temporary function (fun.tmp) to apply kNN over all values in k.grid.
  
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  #Computes classification error for each k.
  #Stores the errors in a named vector.
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  
  ## it will return a list so I need to unlist it to make it to be a vector
  ##Finds the best k (with the lowest error). Returns a list with:
  #k.optimal → best value of k
  #error.min → minimum classification error
  #error.all → all errors for different k values.
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 9, 2), .1)
obj1

## rerun with the best k
ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred1)

#calculate each
sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}
#show them out
sen(ytest, ypred1)
spe(ytest, ypred1)
fpr(ytest, ypred1)
fnr(ytest, ypred1)

#8. Overall Performance Summary
performance = function(ytest, ypred) {
  measures = c(mean(ytest == ypred),
               sen(ytest, ypred),
               spe(ytest, ypred),
               fpr(ytest, ypred),
               fnr(ytest, ypred))
  names(measures) = c('Accuracy', 'Sensitivity', 'Specificity', 'FPR', 'FNR')
  return(measures)
}

performance(ytest, ypred1)

#Variables from Logistic Regression
rm(list=ls()); gc()
setwd('/Users/truct/OneDrive/Desktop/ISDS 574/Team Project/archive')
dat = read.csv('diabetes_binary_health_indicators_BRFSS2015.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = '')

dat$Diabetes_binary = as.numeric(dat$Diabetes_binary)
dat$Diabetes_binary = dat$Diabetes_binary # healthy -> 0; diabetic/pre-diabetic -> 1
dat[, 2:ncol(dat)] = scale(dat[, 2:ncol(dat)])

#2. Splitting Data into Training and Testing Sets
set.seed(1)
n.train = floor( nrow(dat)*0.75 ) #Determines the training set size as 75% of the total dataset.
ind.train = sample(1:nrow(dat), n.train) #Randomly selects training indices and assigns the rest as test indices.
ind.test = setdiff(1:nrow(dat), ind.train)

require(class) #running kNN for Classification
vars = c("HighBP", "HighChol", "BMI", "HeartDiseaseorAttack", 
         "PhysActivity", "Age", "Education", "Income","MentHlth","PhysHlth", "HvyAlcoholConsump",
         "Veggies","Sex","CholCheck","AnyHealthcare","Stroke","DiffWalk")
Xtrain = scale(dat[ind.train, vars])
Xtest = scale(dat[ind.test, vars])
ytrain = as.factor(dat[ind.train, 1])
ypred = knn(Xtrain, Xtest, ytrain, k=3, prob=T) #k=3 neighbors, Uses probability (prob=T) to get estimated class probabilities.
ytest = dat[ind.test,1] #Extracts true labels for the test set and compares them with predictions using a confusion matrix.

get.prob = function(x) {
  prob = attr(x, 'prob')
  ind = which(x == 0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser() Defines a function to determine the best k from a range (k.grid). Uses ct (cutoff threshold) of 0.5 for classification.
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  #Runs kNN for each k and calculates classification errors.
  #Uses a temporary function (fun.tmp) to apply kNN over all values in k.grid.
  
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  #Computes classification error for each k.
  #Stores the errors in a named vector.
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  
  ## it will return a list so I need to unlist it to make it to be a vector
  ##Finds the best k (with the lowest error). Returns a list with:
  #k.optimal → best value of k
  #error.min → minimum classification error
  #error.all → all errors for different k values.
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 9, 2), .1)
obj1

## rerun with the best k
ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred1)

#calculate each
sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}
#show them out
sen(ytest, ypred1)
spe(ytest, ypred1)
fpr(ytest, ypred1)
fnr(ytest, ypred1)

#8. Overall Performance Summary
performance = function(ytest, ypred) {
  measures = c(mean(ytest == ypred),
               sen(ytest, ypred),
               spe(ytest, ypred),
               fpr(ytest, ypred),
               fnr(ytest, ypred))
  names(measures) = c('Accuracy', 'Sensitivity', 'Specificity', 'FPR', 'FNR')
  return(measures)
}

performance(ytest, ypred1)
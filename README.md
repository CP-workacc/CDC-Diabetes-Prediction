# CDC-Diabetes-Prediction

This project uses machine learning models in R to predict whether an individual is **Diabetic**, **Pre-Diabetic**, or **Healthy** based on healthcare and lifestyle indicators from the **CDC Diabetes Health Indicators Dataset**. With a focus on accuracy and interpretability, we evaluate models like KNN, CART, and Logistic Regression under multiple cutoff thresholds to handle class imbalance.

---

## 📊 Project Objectives

- Predict individual diabetes status using supervised classification models.
- Identify top contributing health factors to diabetes.
- Compare the performance of different models using standard evaluation metrics.

---

## 🧾 Dataset Overview

- **Source**: CDC Behavioral Risk Factor Surveillance System (BRFSS)
- **Target variable**: Multiclass — `Diabetic`, `Pre-Diabetic`, `Healthy`
- **Features**: 35 variables, including:
  - Age, BMI, Smoking, Alcohol Use, General Health, Physical Activity, Mental Health, Sleep Hours, etc.

---

## ⚙️ Models Used

- **Multinomial Logistic Regression**  
- **K-Nearest Neighbors (KNN)**  
- **Classification and Regression Tree (CART)**  

Each model was evaluated at different probability cutoffs (0.5, 0.3, 0.1) to analyze performance trade-offs between sensitivity and specificity.

---

## 📦 Tools & Libraries

- **Language**: R  
- **Libraries Used**:  
  - `tidyverse` – data wrangling  
  - `class` – KNN implementation  
  - `rpart`, `rpart.plot` – decision tree (CART) modeling  
  - `nnet` – multinomial logistic regression  
  - `ROCR`, `caret` – model evaluation  
  - `ggplot2` – visualization

---

## 📈 Results Summary

| Model Type          | Strategy                  | Accuracy | Sensitivity | Specificity |
|---------------------|---------------------------|----------|-------------|-------------|
| **Logistic Regression** | Stepwise (Cutoff = 0.5)   | **0.8662** | 0.1669      | **0.9795**   |
|                     | Stepwise (Cutoff = 0.3)    | 0.8436   | 0.4416      | 0.9087      |
|                     | Stepwise (Cutoff = 0.1)    | 0.6647   | **0.8577**  | 0.6334      |
| **KNN**             | Screened variables (Cutoff = 0.5) | 0.8356   | 0.2298      | 0.9343      |
|                     | All variables (Cutoff = 0.1) | 0.8356   | 0.2979      | 0.9343      |
| **Classification Tree** | Best Pruned (Cutoff = 0.3) | 0.8561   | 0.2455      | **0.9549**  |
|                     | Minimum Error (Cutoff = 0.5) | 0.8652   | 0.1350      | **0.9833**  |

### 🔍 Observations
- Logistic Regression yielded the **highest accuracy** (0.8662) at cutoff = 0.5 but had low sensitivity.
- KNN with all variables and lower cutoff performed well in **balancing classes**.
- CART (Classification Tree) models had **excellent specificity** and competitive overall accuracy.

---



# Clear the workspace and free memory
rm(list = ls())
gc()

# Set the working directory
setwd("C:\\Users\\HP\\OneDrive\\Desktop\\Projects\\CDC Diabetes\\archive (3)")

# Load the dataset
dat <- read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\Projects\\CDC Diabetes\\archive (3)\\diabetes_binary_health_indicators_BRFSS2015.csv")

# Check dimensions and structure of the dataset
dim(dat)
str(dat)

### Step 1: Handle Missing Values
library(Amelia)

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

### Step 3: Filter Outliers (example: BMI < 60)
dat2 <- dat[dat$BMI < 60, ]

### Step 4: Visual Check of Continuous Variables (Colorful)
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

### Step 5: Frequency Table for Categorical Variables
table(dat2$GenHlth)

### Step 6: Correlation Matrix with Color
library(corrplot)

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
#Save File
write.csv(dat, "cleaned_diabetes_data.csv", row.names = FALSE)

# Question 1: Data Import and Exploration:

 # I: Load the iris dataset
data("iris")

# II: Display the first two rows
head(iris,2)

# III: Explore the dataset structure using summary() and str()

# View the summary statistics of the dataset
summary(iris)

# View the structure of the dataset
str(iris)


# Question 2: Data Visualisation:

#I: Create a pair plot(scatterplot matrix) to visualize the 
# relationship between the different features (sepal length, sepal width, petal 
# length and petal width)

pairs(iris[, 1:4],
      main = "Scatterplot Matrix of Iris Features",
      pch = 19,
      col = iris$Species)

# II: Use boxplot to visualise the distribution of each feature across three 
# species

# Boxplot for all features

# Arranging plots into two rows and two columns(2x2 Grid)

par(mfrow = c(2,2))

boxplot(Sepal.Length ~ Species, data = iris,
        main = "Sepal Length Distribution",
        col = c("lightblue","lightgreen","pink"))

boxplot(Sepal.Width ~ Species, data = iris,
        main = "Sepal Width Distribution",
        col = c("lightblue","lightgreen","pink"))

boxplot(Petal.Length ~ Species, data = iris,
        main = "Petal Length Distribution",
        col = c("lightblue","lightgreen","pink"))

boxplot(Petal.Width ~ Species, data = iris,
        main = "Petal Width Distribution",
        col = c("lightblue","lightgreen","pink"))

# III: Generate histogram for the numerical values
# (sepal length, sepal width etc)

# Listing out all 4 numerical columns
# Sepal.Length
# Sepal.Width
# Petal.Length 
# Petal.Width 

# Histogram  for Sepal Length 

hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length",
     col = "lightblue",
     border = "white")

# Histogram for Sepal Width 

hist(iris$Sepal.Width,
     main = "Histogram of Sepal Width",
     xlab = "Sepal Width",
     col = "lightgreen",
     border = "white")

# Histogram for Petal Length 

hist(iris$Petal.Length,
     main = "Histogram of Petal Length",
     xlab = "Petal Length",
     col = "lightpink",
     border = "white")

# Histogram for Petal Width

hist(iris$Petal.Width,
     main = "Histogram of Petal Width",
     xlab = "Petal Width",
     col = "lavender",
     border = "white")

# Iv: Implement at least one other plot of your choice that helps
# visualise the data

# Density plot for Sepal Length and Species 

plot(density(iris$Sepal.Length[iris$Species=="setosa"]),
     col="red", lwd=2, main="Density Plot of Sepal Length by Species",
     xlab="Sepal Length")
lines(density(iris$Sepal.Length[iris$Species=="versicolor"]), col="green", 
      lwd=2)
lines(density(iris$Sepal.Length[iris$Species=="virginica"]), col="blue", lwd=2)
legend("topright", legend=c("Setosa","Versicolor","Virginica"),
       col=c("red","green","blue"), lwd=2)



# Question 3: Statistical Summary and Insights:

# I: Calculate Basic Statistical Insight such as Mean, Median and Standard 
# Deviation for each of the Features

# Listing out the Features 

features <- iris[, 1:4]

# Calculating Mean for each features 

sapply(features, mean)

# Calculating Median for each feature

sapply(features, median)

# Calculating Standard Deviation for each feature 

sapply(features, sd)

# II: Provide a brief Interpretation of the Descriptive Statistics
# Interpretation of Descriptive Statistics 

# The sepal dimensions (length and width) are fairly consistent across the 
# iris flowers, with low variability. Petal dimensions (length and width) vary 
# more, especially petal length, reflecting differences between species. 
# This suggests that petal measurements are more useful for distinguishing
# between Setosa, Versicolor, and Virginica, while sepal measurements are 
# relatively similar across species. 
# Overall, the dataset shows moderate and roughly symmetric distributions.

# Below is a Summary Table for each Feature, which supports the descriptive
# statistics discussed above.

stat_summary <- data.frame(
  Feature = colnames(features),
  Mean = sapply(features, mean),
  Median = sapply(features, median),
  SD = sapply(features, sd)
)

stat_summary


# Question 4: Simple Classification Model 

# I: split the dataset into training and test sets
# (e.g 70% training and 30 % testing)

# Setting seed for reproducibility
set.seed(123)

# Shuffling the iris dataset 
iris <- iris[sample(nrow(iris)), ]

# Computing split index for 70% training data
train_index <- 1:round(0.7 * nrow(iris))

# Spliting the dataset into training and testing sets
train_data <- iris[train_index, ]

test_data  <- iris[-train_index, ]

# Checking for Dimensions 
dim(train_data)

dim(test_data)


# II: Implement a K nearest Neighbours(K-NN) classifier to predict thespecies of
# the iris flowers

# Loading the class package 
library(class)

# Preparing the features (X) and (Y) Labels 
train_X <- train_data[, 1:4]
train_Y <- train_data$Species 


test_X <- test_data[, 1:4]
test_Y <- test_data$Species

# Set K (number of Neighbors)
k <- 3

# Build k-NN model and predict species for test set
pred_Y <- knn(train = train_X, test = test_X, cl = train_Y, k = k)

# Display Prediction
pred_Y

# III: Evaluate the accuracy of your model on the test set and display the 
# confusion matrix.

# Accuracy Evaluation
accuracy <- sum(pred_Y == test_Y) / length(test_Y)
accuracy

#Confusion Matrix
confusion_matrix <- table(Predicted = pred_Y, Actual = test_Y)
confusion_matrix


# Iv: Provide a brief Explanation of the K-NN algorithm and interprete the 
#results of the model.

# Explanation of the K-Nearest Neighbors (K-NN) Algorithm

# The K-Nearest Neighbors (K-NN) algorithm predicts the class of a new 
# observation by finding its k closest data points in the training set and by 
# assigning the majority class among those neighbors. 
# It is a simple, distance-based method that works well when similar observations
# belong to the same class.


# Interpretation of the Model results 
# The model achieved 93.33% accuracy, meaning it correctly predicted most iris 
# species. Setosa was classified almost perfectly, while a few errors occurred 
# between Versicolor and Virginica, which have similar measurements. 
# Overall, the K-NN classifier performed very well on the dataset.

# Question 5: Report and Submission Guidelines

# I: Project Details:
# Project Title: Iris Analysis using R programming 
# Group Name:
# Submission Date:

# II: Introduction: Briefly Describe the iris data set and the purpose of the 
# Analysis

# Introduction: 
# The Iris dataset is a well-known dataset in statistics and machine learning,
# it consisting of 150 flower samples from three species
# (Setosa, Versicolor, Virginica). Each sample includes four morphological 
# measurements: sepal length, sepal width, petal length, and petal width. 
# These measurements make the dataset ideal for exploring relationships 
# between features, by performing descriptive statistics, and evaluating simple  
# classification algorithms.

# Purpose of the Analysis:
# The purpose of this analysis is to explore the dataset through descriptive 
# statistics and visualisations, and to build a simple classification model. 
# Specifically, this analysis aims to summarise the key numerical features,
# examine differences across species, and apply a K-Nearest Neighbours (K-NN) 
# algorithm to predict species based on flower measurements.

#III: Methodology:Explain the steps taking to explore , Visualise and Analyse 
#data, including the choice of plots and classification model.

#This analysis of the Iris dataset was conducted in several structured steps:
  
# I: Data Import and Exploration:
# The iris dataset was loaded into R, and its structure was examined using 
# summary() and str() to understand the distribution and types of features.
# The first few rows were displayed with head() to inspect sample observations.

# 2: Data Visualisation:
# Visualisation techniques were employed to explore relationships among features
# and differences across species:
  
# Scatterplot Matrix (Pair Plot): was Used to examine pairwise relationships
# between sepal and petal measurements.


# Boxplots: Illustrated the distribution of each feature across species, 
# highlighting differences in medians and variability.

# Histograms: Showed the frequency distribution of each numerical feature.

# Density Plots: Provided a smoothed representation of feature distributions by
# species.

# 3: Statistical Summary:
# Basic descriptive statistics including mean, median, and standard deviation
# were computed for each feature to summarise central tendency and variability.

# 4: Classification Model:
# A K-Nearest Neighbours (K-NN) classifier was implemented to predict iris
# species based on morphological measurements. The dataset was split into 
# training (70%) and test (30%) sets. The model was evaluated using accuracy 
# and a confusion matrix to assess performance.
# These steps ensured a comprehensive understanding of the dataset and
# demonstrated how simple statistical and machine learning methods can be 
# applied to classify iris species.

# Iv: Results and Discussion: Present and Interprete the key findings including
# Statistical summaries, visualisations and model performance.
# Descriptive statistics and visualisations revealed clear differences among
# iris species: Setosa has the smallest measurements, Virginica the largest, 
# and Versicolor is intermediate. Scatterplots and density plots showed strong 
# correlations between petal length and width, while sepal measurements 
# overlapped slightly. The K-NN classifier achieved 93.33% accuracy, with most 
# errors occurring between Versicolor and Virginica. 
# Overall, the analysis demonstrates that the Iris dataset is suitable for 
# classification, and K-NN effectively predicts species based on flower
# measurements.

# v: Conclusion: Summarise the overal insights gained from the project and any
# recommendation for future analysis.

# This analysis of the Iris dataset provided a comprehensive understanding of 
# differences in sepal and petal measurements across species. 
# Descriptive statistics and visualisations highlighted clear patterns, 
# especially for petal length and width, while the K-Nearest Neighbours (K-NN)
# model demonstrated high predictive accuracy (93.33%).

# For future analysis, it is recommended to explore alternative classification 
# algorithms, such as decision trees or support vector machines, evaluate
# additional metrics (precision, recall, F1-score), and consider feature 
# engineering or dimensionality reduction to improve model performance and 
# interpretability.


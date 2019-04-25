# KNN - K Nearest Neighbour Algorithm
# Non Linear Classifier

#-------------------------
#IMPORTING DATASET
#-------------------------
dataset <- read.csv('Social_Network_Ads.csv')
dataset <- dataset[3:5]

#------------------------------
#SPLIT TRAIN AND TEST SET
#------------------------------
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#--------------------
#FEATURE SCALING
#--------------------
training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

#-----------------------------------------------------
#FIT KNN TO TRAINING SET AND PREDICT TEST SET RESULTS
#-----------------------------------------------------

# Here in knn we don't create the classifier seperately from the vector of 
# prediction y-pred, its done simultaneously

library(class)
y_pred <- knn(train = training_set[,-3],
              test = test_set[,-3],
              cl = training_set[,3],
              k = 5)
y_pred            

#---------------------------------------------
#EVALUATE PREDICTIONS USING CONFUSION MATRIX
#---------------------------------------------

cm <- table(test_set[ ,3], y_pred)
cm

#----------------------------------
#VISUALIZE TRAINING SET RESULTS
#----------------------------------
library(ElemStatLearn)
set <- training_set
X1 = seq(min(set[, 1]) - 1,max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1,max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c('Age','EstimatedSalary')
y_grid <- knn(train = training_set[, -3],
              test = grid_set,
              cl = training_set[, 3],
              k = 5)
plot(set[, -3],
     main = 'Classifier K-NN(Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1), length(X2)),add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3','tomato'))
points(set, pch=21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#----------------------------------
#VISUALIZE TEST SET RESULTS
#----------------------------------

library(ElemStatLearn)
set <- test_set
X1 = seq(min(set[, 1]) - 1,max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1,max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c('Age','EstimatedSalary')

y_grid <- knn(train = training_set[, -3],
              test = grid_set,
              cl = training_set[, 3],
              k = 5)

plot(set[, -3],
     main = 'Classifier K-NN(Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1), length(X2)),add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3','tomato'))
points(set, pch=21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




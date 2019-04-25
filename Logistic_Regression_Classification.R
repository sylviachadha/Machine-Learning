# Logistic Regression is a LINEAR CLASSIFIER, WILL LINEARLY
# SEPERATE 2 CLASSES OF USERS
#---------------------------------------------
# get probability of outcome as well as predicted o/p yes or no
#---------------------------------------------------------------
# uses Probability as a score so u can actually
# rank who is most likely to take up action who is least likely to 
# take beter than having 1 or 0 u have probability

# Instead of probability u can also ask for prediction of y value
# Just assume line at 50% and if outcome prob less than 50% we assume
# 0 line not take offer n if more than 50% then take offer

# Trying to fit best curve as per formula to r data, similar to 
# linear regression we best fit line as per line equation to data
# We get model and coefficients n we start drawing insights from
# this and some of these insights include we can get a probability 
# of somebody taking any action of event occuring and also we can
# get predicted value of o/p variable based on where we select the
# arbitrary line (here we took at 50%)

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
# For Classification it is better to do feature scaling
training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

#--------------------
#BUILDING MODEL
#--------------------
classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training_set)
# For logistic regression u need to specify binomial family

#----------------------------
#PREDICTING TEST SET RESULTS
#----------------------------
# We r interesting in getting 0 and 1 predictions rather than
# probabilities but we first predict probabilities. here type = reponse

prob_pred <- predict(classifier, 
                     type = 'response',
                     newdata = test_set[-3])

prob_pred

# We need to get ans in 1 or 0 i.e.purchased or not purchased
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
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
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic regression(Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
     
contour(X1,X2,matrix(as.numeric(y_grid),length(X1), length(X2)),add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3','tomato'))
points(set, pch=21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Red points r training set observations for which dependent variable 
# purchased is 0 and green r training set observations for which dependent 
# variable purchased is 1 so 2 classes.
# Interpretation - users who were young in age with low estimated salary
# did not buy the SUV. users which are older and high in salary mostly
# buy these SUV's
# Also some older people with low estimated salary bought the SUV. also
# there r some young people with high salary who bought SUV

# The goal is to classify the right users into right category
# Red n green r prediction regions and red n green points are actual obdervations
# so if red in red region means correct but if red in green region means
# incorrect

# There is a prediction boundary b/w red and green prediction regions
# this prediction boundary is a straight line, this is a straight line
# cz logistic regression is a linear classifier. if we were in 3 dimensions
# it would be a straight plane seperating 2 spaces but in 2 dimensions it 
# is a stright line n it will always be a straight line if ur classifier
# is a linear classifier


# Later when we build non linear classifiers then prediction boundary 
# seperator will not be a straight line

#----------------------------------
#VISUALIZE TEST SET RESULTS
#----------------------------------

# the regions decided by classifier will be same, now only the test points 
# will be checked- boundaries remain same once made by classifier acc to
# logistic regression formula

library(ElemStatLearn)
set <- test_set
X1 = seq(min(set[, 1]) - 1,max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1,max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c('Age','EstimatedSalary')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic regression(Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1), length(X2)),add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3','tomato'))
points(set, pch=21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))










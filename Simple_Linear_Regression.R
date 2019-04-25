# Simple Linear Regression
# y = b0 + b1*x1 equation of line on x-y axis where y is Dependent Variable
# (output) and x is independent variable (input). b1 is a coefficient which means its
# not always the case that x & y change in same proportion but they might change 
# in different proportions. b0 is a constant term.

# Constant means the point where the line crosses the vertical axis.0 on 
# x axis means 0 years of experience so constant referring to starting salary

# b1 is slope of line. if coefficient is small then slope will be small
# and less salary increase per year of experience, if slope greater then
# experience will yield a more increase in salary.

# We are using the observations that we have to find the best fitting line.

# There are 2 values one which is actual (yi)and other which is a modelled
# value (y^i)

# How to find best fitting line ? OLSM - Ordinary Least Square Method
# Linear regression draws all possible trend lines through ur data and 
# counts the sum of those squares [actual - observed] every single time
# n records it somewhere in a temporary file etc and then it finds the 
# minimum one so it looks for minimum sum of squares
# It finds a line which has the smallest sum of squares possible and that
# line will be the best fitting line and that is called as Ordinary Least
# Square method -----SUM of (y-y^)square----- - > Minimum
 
#-------------------------
#IMPORTING DATASET
#-------------------------
dataset <- read.csv('Salary_Data.csv')

#------------------------------
#SPLIT TRAIN AND TEST SET
#------------------------------
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#------------------------------
#SIMPLE LINEAR REFRESSION MODEL
#------------------------------

# Simple linear model regression package in R takes care of feature scaling 
# hence no need to do in data preprocessing explicitly

# Before making the model need to identify which is the independent variable
# (Years of Exxperience) and which is dependent variable (Salary)

# lm is used to fit linear models. It can be used to carry out regression
# Formula - Dependent variable expressed as a linear combination of 
# independent variable so here it says Salary is propotional to YearsExperience
regressor <- lm(formula = Salary ~ YearsExperience,
                data = training_set)
summary(regressor)

# 3 stars in Significance code means it is highly statistically significant
# so that means we know Salary and YearsExperience are highly correlated
# U can choose independent variables by checking how stitistically significant 
# they are using Significance code

# Lower p value means more statistically significant ur independent variable
# is going to be that means it will have more effect on ur dependent variable

# Good thresohold for p value is 5 %. If p value is less than 5% means more
# significant ur independent variable is more than 5% means less significant

#------------------------------
#PREDICT RESULTS ON TEST SET
#------------------------------
y_pred_test <- predict(regressor, newdata = test_set)
y_pred
y_pred_train <- predict(regressor, newdata = training_set)

#------------------------------
#VISUALIZE TRAINING SET RESULT
#------------------------------
#geom_point to make a scatterplot of all observation points
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = y_pred_train),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of experience') +
  ylab('Salary')


#------------------------------
#VISUALIZE TEST SET RESULT
#------------------------------
#geom_point to make a scatterplot of all observation points
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = y_pred_train),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')  

# In goem_line that is the component that builds the regression line, our
# regressor is already trained on the training set so whether we keep here 
# training_set or replace by test_set we will obtain the same simple linear 
# regression line. If we replaced we would just build some new points of
# the same regression line corresponding to the new predictions of the test
# set observation points cz when we trained simple linear regressor on the
# training set we obtained 1 unique model equaltion which is simple linear
# equation itself and therefore if we build regression line here by predicting
# the training set points or the test set points, well since this prediction
# results from the same, unique simple linear equation we will get the 
# the same regression line

# If u compare 2 regression results for train and test set u will observe 
# that only the observation points change but the regressor line remains
# the same

# Most of the points are near to regressor line in test that means for
# sure to quite an extent there is linear dependency but some points are
# also far that means there is not 100% linear depdndency b/w Salary
# and years of experience






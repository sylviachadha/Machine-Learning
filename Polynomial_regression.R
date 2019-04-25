# Polynomial_regression - to solve non linear problems

# Basically to build non linear regression models where in datasets there is not a 
# linear relationship b/w the independent and dependent variables.

# Its similar to MLR but at same time instead of different variables like x1,x2, x3, xn
# we have same variable x1 but it is in different powers like instead of x2 have x1
# square. formula y = b0 + b1x1 + b2x1square + b3x1cube + ..
# square n cube give the parabolic effect to line and we get a curve 
# Depends on use case like they r sometimes used to describe how diseases or epidemics 
# spread across population

# why still called as polynomial linear regression inspite of square, cube in degrees 
# etc this is cz when we r talking about linear n non linear we r not actually
# talking about the x variables although the function here y w.rt x is non linear
# but when we talk about class of regression whether its linear or non linear u r
# talking about the coefficients here b0,b1,b2 .. bn which r linear
# y is a function of x which is non linear but the ques here is whether this function  can be 
# can be expressed as a linear combination of these coefficients cz ultimately they
# r the unknowns so ur goal when u r building regression is to find these coefficients,
# find out there actual values so further down track u can use those coefficients to
# plug in x and predict y whether it is single, multiple, polynomial linear regression

# So linear, non linear refer to the coefficients.
# Polynomial linear regression is a special case of multiple linear regression
# rather than a standalone or totally new type of regression.

#-------------------------
#IMPORTING DATASET
#-------------------------
dataset <- read.csv('Position_Salaries.csv')
# We select only 2 columns in this dataset for training col as 1,2 same
dataset <- dataset[2:3]

#------------------------------
#SPLIT TRAIN AND TEST SET
#------------------------------
# We have only 10 observations so we do not split into training 
# and test set

#------------------------------
#FEATURE SCALING
#------------------------------
# A Polynomial regression model is actually a MLR model with polynomial
# terms, instead of having different features we r taking 1st 
# feature and taking different powers of it

#----------------------------------------------
#BUILD POLYNOMIAL NON LINEAR REGRESSION MODEL
#----------------------------------------------
# This polynomial regression model will learn correlations bw level
# salary n be able to predict on new values

# To show for this data polynomial linear regression is a most suite
# and powerful model we compare it with simple linear regression model

#----------------------------------------------
#FITTING LINEAR REGRESSION TO DATASET
#----------------------------------------------

lin_reg <- lm(formula = Salary ~ .,
              data = dataset)
summary(lin_reg)

y_pred_lin <- predict(lin_reg, newdata = dataset)

#----------------------------------------------
#FITTING POLYNOMIAL REGRESSION TO DATASET
#----------------------------------------------
# PRM is actually a MLR model in which independent variables
# r actually polynomial features of 1 independent variable, so
# use same lm function. 
# First build polynomial terms and then just apply to it same MLR model
dataset$Level2 <- dataset$Level^2
dataset$Level3 <- dataset$Level^3
dataset$Level4 <- dataset$Level^4

# Now apply MLR to original independent variable and the polynomial 
# term created
poly_reg <- lm(formula = Salary ~ .,
               data = dataset)
summary(poly_reg)

y_pred_poly <- predict(poly_reg, newdata = dataset)

#-----------------------------------------
#VISUALISING LINEAR REGRESSION MODEL
#-----------------------------------------
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = y_pred_lin),
            colour = 'blue') +
  ggtitle('Truth or Bluff(Linear Regression)') +
  xlab('Level') +
  ylab('Salary')
  
# Each time u build a linear model in 2 dimensions u get a straight
# line. Blue line is not fitting our observation v well
# If non linear model it is not a straight line'

#-----------------------------------------
#VISUALISING POLYNOMIAL REGRESSION MODEL
#-----------------------------------------
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = y_pred_poly),
            colour = 'blue') +
  ggtitle('Truth or Bluff(Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#---------------------------------------------
#PREDICTING NEW RESULT WITH LINEAR REGRESSION
#---------------------------------------------
# Not predict on whole test set but make a single prediction
y_pred_test_lin <- predict(lin_reg, data.frame(Level = 6.5))

y_pred_test_lin

#-------------------------------------------------
#PREDICTING NEW RESULT WITH POLYNOMIAL REGRESSION
#-------------------------------------------------
# Polynomial learnt not only on Level but also Level2,3,4
y_pred_test_poly <- predict(poly_reg, data.frame(Level = 6.5,
                                                 Level2 = 6.5^2,
                                                 Level3 = 6.5^3,
                                                 Level4 = 6.5^4))

y_pred_test_poly














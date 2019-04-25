# Multiple_Linear_Regression

# y = b0 + b1*x1 + b2*x2 + .... + bn*xn
# y is dependent variable and x1,x2, .... ,xn are independent variables
# b0 is constant and b1,b2 ... bn are coefficients

# A categorical variable cannot be added to equation so you need to check 
# on categorial variables in regression models by creating dummy variables
# First find all categories in categorical column & create new columns for
# it which will be dummy variables columns, so we r expanding r dataset & 
# adding some new columns to it then populate with 0 and 1 based on presence
# / absence. Dummy variables act like switches 0 or 1 value only

# Whenever u r changing categorical to dummy variable then if it has 3 
# states u take only 2 dummmy variables, if 2 take 1, if 4 take 3, so
# it is always 1 variable less than no of states

# Null hypothesis (H0) is one against which we r trying to provide the evidence
# So null hypothesis is as it should be weight of peanuts is 70g. Alternative
# Hypothesis is called H1 or HA is what we r tring to prove. customers claim 
# that wt of peanuts is less than 70g which we want to test to alternative
# hypothesis is wt of peanuts is less than 70g

# Significance level of 5% or 0.05. If p value is lower than this she will
# reject the null hypothesis. p value is found to be .18 that is 18% which
# is greater than 5% so null hypothesis cannot be rejected.
# If p value would have been lower than 5% then alternative hypothesis would
# have been statistically significant and null hypothesis would have been 
# rejected
# P IS LOW, NULL MUST GO. The smaller the p value the more evidence we
# have that null hypotheis is probably wrong.This is statistically significant 
# result else if u cannt reject the null hypothesis means everything
# normal no new knowledge so this is a nonsignificant result.

# To build the right model u need to select the right variables out of 100
# maybe choose just 20 which r significant and explanable

# 5 methods of building models -> All-in, Backward Elimination, Forward
#------------------------------------------------------------------------
# Selection,Bidirectional Elimination, Score Comparison
#-------------------------------------------------------

# 1.All Possible Methods - most resource consuming approach, not a good
# approach, no of models grow exponentially

# 2.In backward elimination u keep on removing variables / feature one by one
# based on highest p value i.e that will be least significant and u do this
# until u r left with feature with even the highest p value is less than
# the significance level so u now no more want to eliminate it

# 3.Forward Selection
# 4.Birectional Elimination also called as stepwise regression.
# 5.Score Comparison

#-------------------------
#IMPORTING DATASET
#-------------------------
dataset <- read.csv('50_Startups.csv')

#--------------------------
#ENCODING CATEGORICAL DATA
#--------------------------
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

#-----------------------------
#SPLIT TRAIN AND TEST SET
#-----------------------------
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#--------------------
#FEATURE SCALING 
#--------------------
# This will be automatically taken care of by the function we are going
# to use to fit multiple linear regression to r training set.

#-----------------------
#MODEL ON TRAINING DATA
#-----------------------
# Fitting Multiple Linear regression model to training set
# Profit is going to be a linear combination of independent variables in case of MLR
# If only 1 independent variable we say profit is propotional to that variable

# R replaces spaces by dots
regressor <- lm(formula = Profit ~ .,
                data = training_set)
summary(regressor)
# State2 and State3 are dummy variables created by R and since there were
# 3 categories it created 2 dummy variables

# Coefficient , Std Error, t value, p value, Significance Level are in 
# summary of regressor
# p value and Significance level are most imp cz they tell us about the 
# statistical significance of the indepdndent variables onto the dependent
# variable
# Lower the p value, the more statistically significant your independent
# variable is going to be i.e it will have more effect on dependent variable
# Good threshold is 5%, so in this case among all predictors R.D is most
# signoficant, rest are kind of unuseful cz not even 1 * or . with them

# So we can narrow down the features to only R.D.Spend which would give
# same result
#regressor <- lm(formula = Profit ~ R.D.Spend,
#               data = training_set)

#-----------------------
#PREDICTING ON TEST SET
#-----------------------
y_pred <- predict(regressor, newdata = test_set)
y_pred

#---------------------------------------------------
# BUILDING OPTIMAL MODEL USING BACKWARD ELIMINATION
#---------------------------------------------------

# Find an optimal team of independent variables so each IV of team has
# great impact/statistically significant on the Dependent variable Profit
# Impact can be +ve or -ve

regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                data = dataset)
summary(regressor)

# We will remove each IV which is not statistically significant one by one
# as per Backward elimination process
# State removed

regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = dataset)
summary(regressor)

# Admininstration removed

regressor <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)
summary(regressor)
# Now Marketing.Spend comes with a . which means it lies b/w and 5% and 
# 10% so it is an arbitrary call to keep it or not but since we have
# chosen a significance level of 5% so above this p value we should 
# eliminate, here we r strictly following backward elimination but there
# r other factors as well which contibute to decision which we focus later

# So that means as u keep on removing 1 by 1 weightage of other variables
# r effected, might become more or less significant

regressor <- lm(formula = Profit ~ R.D.Spend,
                data = dataset)
summary(regressor)









#-----------------------
#DATA PREPROCESSING
#-----------------------

#------------------------
#IMPORTING LIBRARIES
#------------------------
#install.packages('caTools')
library('caTools')

#-------------------------
#IMPORTING DATASET
#-------------------------
dataset <- read.csv('Data.csv')
#If need to select particular columns of dataset 
#dataset <- dataset[, 2:3]

#--------------------------------
#TAKING CARE OF MISSING DATA
#--------------------------------
#1. First option is to remove rows which have missing data
#2. Take mean of columns for example there is one missing value in 
#age column, then u take mean of that age column and put it in that
#missing value cell

dataset$Age <- ifelse(is.na(dataset$Age),
                      ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$Age)

#is.na(dataset$Age) -> condition
#ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),-> If true
#dataset$Age) -> If not true

dataset$Salary <- ifelse(is.na(dataset$Salary),
                         ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                         dataset$Salary)

#------------------------------
#ENCODING CATEGORICAL DATA
#------------------------------
#2 categorical variables here Country and Purchased Variable
#Called categorical cz contain categories like yes/ no OR
#categories of countries like France, Spain, Germany
#MC Learning models r based on mathematical equations so we need to
#encode categorical into numbers

#Factor function will convert your categorical variable to numeric
#categories and u can choose labels for those fcators

dataset$Country <- factor(dataset$Country,
                          levels = c('France','Spain','Germany'),
                          labels = c(1,2,3))

dataset$Purchased <- factor(dataset$Purchased,
                          levels = c('No','Yes'),
                          labels = c(0,1))

#------------------------------
#SPLIT TRAIN AND TEST SET
#------------------------------
#Performance on test set should almost be similar to train set
#which means model did not learn by heart the correlations and is 
#able to generalize

set.seed(123)

#split will return either true or false, true means sample goes to
#training set and false means sample/observation goes to test set

split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
#[1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#--------------------
#FEATURE SCALING
#--------------------
#Can apply only to numeric columns and not to factor as factor is
#not considered as numeric, it is just numeric categories
#This is done cz most of m/c learning models r based on Eucleidien 
#distance and square of 1 col will be negligible in comparison to
#square of other col if range difference is huge in both columns
#like in this case age and salary hence need to do feature scaling

#Shift+command+C for commenting all lines together
training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])












#Done by Harini G
#Title: "THE SPARKS FOUNDATION"
#Question:Predict the percentage of a student based on the no. of study hours.

#Importing the Packages
library(dplyr)
library(ggplot2)
library(corrplot)

#importing the dataset from the provided dataset
data = read.csv(url("http://bit.ly/w-data"))
head(data)

#dimension the dataset
dim(data) #the dataset cointains 2 features and 25 observations.

#finding the Column names
colnames(data) #Hours and Scores are the two variables present in the dataset.

#DATA PREPROCESSING

#checking for NAN values
colSums(is.na(data)) #The given dataset contains no NaN values.

#checking NULL values
is.null(data) #The given dataset contain no NULL values.

#EXPLORATORY DATA ANALYSIS

#structure of the dataset
str(data) #The variable Hours is of type Numeric and Scores is of datatype Integer.

#SUMMARY OF THE DATASET
summary(data)
#The Minimum value of Hours is 1.100 and maximum value is 9.200. Mean value is greater than median value. Hence it is right skewed.
#The Minimum value of Scores is 17.00 and maximum value is 95.00. Mean value is greater than median value. Hence it is right skewed.

#BOXPLOT
# Checking the outliers
boxplot(data,main='Boxplot of Hours and Scores')
#From the boxplot, we can understand that No outliers are present in the dataset.

#SCATTERPLOT
my_graph <- ggplot(data,
                   aes(x =Hours, y =Scores))+
  ggtitle('HOURS V/S SCORES')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(col='orange') 
my_graph
#From the graph we can see that there is a linear relationship between the response 
#variable and explanatory variable(i.e., linear relationship between Scores and Hours). 
#Also the direction of association seems to be positive i.e. As Hours increase, the 
#Scores obtained also increase and vice-versa.

#CORRELATION
corrplot(cor(data),
         method ='number',
         type = 'upper' # show only upper side
)

#using pearson correlation
cor.test(data$Hours,data$Scores)
#Here, the correlation value is 0.9761907. Hence, we can understand that there exists 
#a high positive correlation between Hours and Scores.

#DATA MODELLING

#Train Test Splitting
set.seed(100)
rows=sample(nrow(data))

#Randomly order data
data=data[rows,]
#Identify row to split on: split
split = round(nrow(data) * .80)
#Create train
train=data[1:split,]
#Create test
test=data[(split+1):nrow(data),]
#dimension of train and test dataset
dim(train) #train dataset contains 20 observations and 2 variables.
dim(test) #test dataset contains 5 observations and 2 variables.

#Linear Regression Model

#fitting linear regression model
linmod = lm(Scores~Hours, data = train)

#taking the summary of the model
summary(linmod)
#The value of intercept of the linear model is 3.5030. The slope of the model is 9.4682.
#Hence,the model can be interpreted as :
#Scores = 9.4682 * Hours + 3.5030. 
#Residual standard error is the measure of the quality of the linear regreesion fit and here it is 5.508 on 18 degrees of freedom.
#R squared statistic provides measure of how well the model is fitting the actual dataset.Here, 94percent of fitting to the linear model.
#F statistic value is 311.3, which is relatively larger than 1. Hence, a good relationship is existing between Sales and Spend.

#Predicting the Scores
Pred = predict(linmod, test)

#Comparing The Actual and Predicted Scores
data.frame(Actual=test$Scores,Predicted=Pred)

#Comparing The Actual and Predicted Values using Data Visualisation
my_graph <- ggplot(train,
                   aes(x =Hours, y =Scores))+
  ggtitle('Actual V/S Predicted')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(col='orange')+
  stat_smooth(method = 'lm',
              col = 'green',
              se = FALSE,
              size = 1)
my_graph

#What will be the predicted score if the student studies for 9.25 hr/day?
test2 = data.frame(Hours = 9.25)
predict(linmod, test2)
#Therefore, according to the regression model, if a student studies for 9.25 hours 
#per day he/she is likely to score 91.08419.
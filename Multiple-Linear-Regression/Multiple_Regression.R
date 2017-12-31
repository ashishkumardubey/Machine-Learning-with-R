# Importing the dataset
dataset = read.csv('50_Startups.csv')
class(dataset)
str(dataset)
# Spilitting the dataset in Training and Test datasets

#Taking care of the Categorical data by Encoding it

dataset$State = factor(dataset$State,
                         levels= c('New York','California','Florida'),
                         labels = c(1,2,3))


#install.packages('caTools')
#library(caTools)
set.seed(123)
split=sample.split(dataset$Profit,SplitRatio = 0.7)
split
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)



#Fitting the model

regressor = lm(formula = Profit~.,data = training_set)
summary(regressor)

regressor = lm(formula = Profit~R.D.Spend,data = training_set)
summary(regressor)


#Predict the test set results

y_pred=predict(regressor,newdata = test_set)
y_pred

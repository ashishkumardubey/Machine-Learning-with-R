# Importing the dataset
dataset = read.csv('Salary_Data.csv')
class(dataset)
str(dataset)
# Spilitting the dataset in Training and Test datasets

#install.packages('caTools')
#library(caTools)
set.seed(123)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
split
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)


#Fitting the Linear Regression Model to Training Dataset

regressor<- lm(formula = Salary~YearsExperience,data = training_set)
regressor 
summary(regressor)


#Predicting  the Test Dataset results

y_pred<-predict(regressor,newdata = test_set)


# Visualising the Training set results

library(ggplot2)

ggplot()+
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),colour="blue")+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),colour="black")+
  ggtitle("Salary vs Years of Experience(Training_Dataset)")+
  xlab("YearsofExperience")+
  ylab("Salary")


# Visualising the Test set results



ggplot()+
  geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),colour="red")+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),colour="black")+
  ggtitle("Salary vs Years of Experience(Test_Dataset)")+
  xlab("YearsofExperience")+
  ylab("Salary")

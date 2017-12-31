# Importing the dataset
dataset = read.csv('Position_salaries.csv')
class(dataset)
str(dataset)


#Dropping the first column since it is not relevant for the Regression

dataset=dataset[-1]


#Fitting the SVR model
install.packages("e1071")
library(e1071)
lin_reg = svm(formula =Salary~Level,data = dataset,type="eps-regression" )
summary(lin_reg)


library(ggplot2)

#Visualising the SVR results
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary), color="Red")+
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata=dataset)),color="Blue")+
  ggtitle("Truth or Bluff(SVR)")+
  xlab("Levels")+
  ylab("Salary")



#Predicting a new result with SVR

y_lin_pred= predict(lin_reg,data.frame(Level=6.5))




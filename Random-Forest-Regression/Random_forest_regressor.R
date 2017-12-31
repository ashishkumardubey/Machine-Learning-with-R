# Importing the dataset
dataset = read.csv('Position_salaries.csv')
class(dataset)
str(dataset)


#Dropping the first column since it is not relevant for the Regression

dataset=dataset[-1]


#Fitting the Random forest model
library(randomForest)
set.seed(1234)
lin_reg = randomForest(x=dataset[1],y=dataset$Salary,ntree = 300)
summary(lin_reg)


library(ggplot2)

#Visualising the SVR results

x_grid=seq(min(dataset$Level),max(dataset$Level),0.1) # Changing the grids for high level resolution graph


ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary), color="Red")+
  geom_line(aes(x=x_grid,y=predict(lin_reg,newdata=data.frame(Level=x_grid))),color="Blue")+
  ggtitle("Truth or Bluff(Random Forest Regression)")+
  xlab("Levels")+
  ylab("Salary")



#Predicting a new result with SVR

y_lin_pred= predict(lin_reg,data.frame(Level=6.5))




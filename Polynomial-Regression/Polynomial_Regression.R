# Importing the dataset
dataset = read.csv('Position_salaries.csv')
class(dataset)
str(dataset)


#Dropping the first column since it is not relevant for the Regression

dataset=dataset[-1]


#Fitting the linear regression model

lin_reg = lm(formula =Salary~Level,data = dataset)
summary(lin_reg)


#Fitting the Polynomial Regression Model
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4


poly_reg = lm(formula =Salary~.,data = dataset)
summary(poly_reg)

library(ggplot2)

#Visualising the Linear regression results
ggplot()+
geom_point(aes(x=dataset$Level,y=dataset$Salary), color="Red")+
geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata=dataset)),color="Blue")+
ggtitle("Truth or Bluff(Linear Regression")+
xlab("Levels")+
ylab("Salary")






#Visualising the polynomial Regression Results
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary), color="Red")+
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata=dataset)),color="Blue")+
  ggtitle("Truth or Bluff(Polynomial Regression")+
  xlab("Levels")+
  ylab("Salary")


#Predicting a new result with linear Regressiom

y_lin_pred= predict(lin_reg,data.frame(Level=6.5))


#Predicting a new result with Polynomial regression

y_poly_pred=predict(poly_reg,data.frame(Level=6.5,
                                        Level2 = 6.5^2,
                                        Level3 = 6.5^3,
                                        Level4 = 6.5^4))

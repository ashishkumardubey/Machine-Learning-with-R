# Importing the dataset

dataset = read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
str(dataset)
dataset$Purchased= factor(dataset$Purchased)# Since it was not factor
str(dataset)

#Splitting the dataset into training and test datasets

library(caTools)
set.seed(123)

split=sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
table(training_set$Purchased)/nrow(training_set)
table(test_set$Purchased)/nrow(test_set)

#Feature Scaling

training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])


# Fitting the NAIVE BAYES Model

library(e1071)
classifier= naiveBayes(x= training_set[-3],y = training_set$Purchased)
summary(classifier)


# Making the predicitions
y_pred = predict(classifier,newdata=test_set[-3])
y_pred


# making the confusion matrix

cm = table(test_set[,3],y_pred)
cm




# Visualising the results

#install.packages("ElemStatLearn")
library(ElemStatLearn)

set=test_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set) = c('Age','EstimatedSalary')

y_grid=predict(classifier,newdata = grid_set)
plot(set[,-3],
     main= 'Naive Bayes Classifier(Test Set)',
     xlab= "Age",ylab= "EstimaedSalary",
     xlim= range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)), add = T)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4','red3'))


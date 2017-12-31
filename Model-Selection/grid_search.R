# Importing the dataset

dataset = read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]



#Splitting the dataset into training and test datasets

library(caTools)
set.seed(123)

split=sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
table(training_set$Purchased)/300
table(test_set$Purchased)/100

#Feature Scaling

training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])


# Fitting the Kernal SVM Model

library(e1071)
classifier= svm(formula= Purchased~., data= training_set, type = 'C-classification' , kernel='radial')
summary(classifier)


# Making the predicitions
y_pred = predict(classifier,newdata= test_set[-3])



# making the confusion matrix

cm = table(test_set[,3],y_pred)
cm


# Using K-Fold Cross Validation Performance Evaluation
library(caret)
folds=createFolds(training_set$Purchased,k=10)
cv=lapply(folds,  function(x){
  training_fold=training_set[-x,]
  test_fold=training_set[x,]
  classifier_1= svm(formula= Purchased~., data= training_fold, 
                    type = 'C-classification' , kernel='radial')
  
  y_pred_1 = predict(classifier_1,newdata= test_fold[-3])
  
  cm_1= table(test_fold[,3],y_pred_1)
  accuracy=(cm_1[1,1]+cm_1[2,2])/(cm_1[1,1]+cm_1[1,2]+cm_1[2,1]+cm_1[2,2])
  return(accuracy)
  
})
mean(as.numeric(cv))

# Applying Grid Search
classifier=train(form = Purchased~.,data = training_set,method= 'svmRadial')
classifier
classifier$bestTune

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
     main= 'Kernel SVM Classifier(Test Set)',
     xlab= "Age",ylab= "EstimaedSalary",
     xlim= range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)), add = T)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4','red3'))


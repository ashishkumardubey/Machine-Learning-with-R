# Importing the dataset

dataset_1=read.csv('Restaurant_Reviews.tsv', sep = '\t',quote = "", stringsAsFactors = FALSE)
str(dataset)

# Cleaning the dataset
#install.packages('SnowballC')
library(NLP)
library(tm)
library(SnowballC)
#creating a corpus of reviews
corpus=VCorpus(VectorSource(dataset$Review))

# access 1st row of corpus
as.character(corpus[[1]])
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,content_transformer(removeNumbers))
corpus=tm_map(corpus,content_transformer(removePunctuation))
corpus=tm_map(corpus,content_transformer(removeWords),stopwords())

# Stemming the corpus
corpus=tm_map(corpus,stemDocument)

# Remove extra Spaces between words
coprus=tm_map(corpus,stripWhitespace)


# Creating  the BAG of Words Model

dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.999)
dtm
dataset=as.data.frame(as.matrix(dtm))
dataset$Liked=dataset_1$Liked


# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))


#Splitting the dataset into training and test datasets

library(caTools)
set.seed(123)

split=sample.split(dataset$Liked, SplitRatio = 0.85)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
table(training_set$Liked)/nrow(training_set)
table(test_set$Liked)/nrow(test_set)

# Fitting classifier to the Training set
# Create your classifier here
library(randomForest)
classifier=randomForest(x= training_set[-692],y = training_set$Liked,ntree= 10)


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm

# model accuracy =sum of correct predictions/nrow(test_set)
(61+56)/nrow(test_set)


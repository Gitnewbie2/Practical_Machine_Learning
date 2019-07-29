#Source: http://groupware.les.inf.puc-rio.br/har :)

library(caret)

#Load training dataset
trainfile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training <- read.csv(trainfile, header=TRUE)
training$classe <- as.factor(training$classe)

#Load testing dataset
testfile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testing <- read.csv(testfile, header=TRUE)

#See variable list
names(training)
names(testing)

#Drop first 7 variables since not useful in prediction
training <- training[,8:length(colnames(training))]
testing <- testing[,8:length(colnames(testing))]

# Drop colums with NAs
training <- training[, colSums(is.na(training)) == 0] 
testing <- testing[, colSums(is.na(testing)) == 0] 

# Drop variables with zero variance predictors
nzv <- nearZeroVar(training,saveMetrics=TRUE)
zero.var.ind <- sum(nzv$nzv)

if ((zero.var.ind>0)) {
    training <- training[,nzv$nzv==FALSE]
}

#Create Cross Validation dataset
inTrain <- createDataPartition(training$classe, p=0.70, list=F)
training_final <- training[inTrain, ]
training_cv <- training[-inTrain, ]

#Train model using Random forest
mod_rf <- train(classe ~., data=training_final, method="rf", trControl=trainControl(method="cv", number=5))
pred_rf <- predict(mod_rf, training_cv)
confusionMatrix(pred_rf,training_cv$classe)

#Predict on test dataset
results <- predict(mod_rf, testing[, -length(names(testing))])
results

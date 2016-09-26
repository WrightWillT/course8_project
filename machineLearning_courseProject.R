train_raw1<-read.csv("pml-training.csv", header = T, sep = ",")
test_raw1<-read.csv("pml-testing.csv", header = T, sep = ",")

library('caret')
library('rpart')
library('e1071')
library('randomForest')
sum(complete.cases(train_raw1))
#there are 406 obs of the 19,622 that have complete cases

THRESHOLD<-0.1
#THRESHOLD is the constant for the percent NA's in a column acceptable for inclusion in the algorithm
sum(colSums(is.na(test_raw1))/nrow(test_raw1)>THRESHOLD)
#testing different values for the THRESHOLD revealed that variables are all-or-nothing NAs
#100 variables in the test dataset contain only NAs and thus should be excluded from the trainig and test sets

test_raw2 <- test_raw1[, colSums(is.na(test_raw1)) == 0] 
#now we remove the variables that obviously won't be imporant for the algorithm like the X (observation number), user_name,
#timestamps, and windows.  These are variables 1:7 so we'll just exclude those:
test_final<-test_raw2[,8:60]

#we now have 52 possibly useful variables and a 53rd that's the problem_id; we'll grab the 52 for the training set and keep the classe:
train_raw2 <- train_raw1[, c(colnames(test_final)[1:52],"classe")] 

#set seed to ensure reproducibility:
set.seed(7223)
in_train <- createDataPartition(train_raw2$classe, p=0.7)[[1]]
training <- train_raw2[in_train,] 
testing <- train_raw2[-in_train,]

#create a random forest model:
mod_rf<- train(classe~., data = training, method = "rf")
#create prediction based on model and cross-validate:
pred <- predict(mod_rf,testing)
confusionMatrix(testing$classe, pred)

#use model to predict the 20 test cases:
test_results <- predict(mod_rf,test_final)
test_results


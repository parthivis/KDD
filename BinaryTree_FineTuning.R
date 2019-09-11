library(tictoc)
options(warn =-1)
library(tictoc)
tic("time taken")## for calculating the time the algorithm is taking to build a model
Gradadmit <- read.csv(file = "C:\\Users\\Pranjal\\Desktop\\Summer2019\\Project\\datafinal.csv",header = TRUE,sep = "," , na.strings = "?")##Reading the data
library(caTools)##used for moving window statistics, GIF, Base64, ROC AUC, etc.
set.seed(88)
split <-sample.split(Gradadmit$Decision ,SplitRatio = 0.8)##Splitting the data to the ratio of 80% training and 20% testing data
Training_Data <- subset(Gradadmit,split == TRUE)## Training data
Testing_Data <- subset(Gradadmit,split  == FALSE)## Testing data
prop.table(table(Training_Data$Decision))## combined with table () to verify if the randomization process is correct.
prop.table(table(Testing_Data$Decision))## combined with table () to verify if the randomization process is correct.
library(rpart)## to build binary tree model using rpart() library.
Model <- rpart(Decision~., data = Training_Data, method = 'class')## building a training model using Binary algorithm.
#library(rpart.plot)
#rpart.plot(Model, box.palette="RdBu", shadow.col="gray", nn=TRUE,extra = 106)
predict_unseen <-predict(Model, Testing_Data, type = 'class')## Using training model to predict test set
table_mat <- table(Testing_Data$Decision, predict_unseen)## Create a table to count how many passengers are classified as acceptance & rejection and passed away compare to the correct classification
base_accuracy <- sum(diag(table_mat)) / sum(table_mat)## To calculate the accuracy of the model.
print(paste('Accuracy for Model', base_accuracy))## for printing the accuracy of the model.
## PrePruning of the tree
Model_preprun <- rpart(Decision~., data = Training_Data, method = "class", control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
predict_unseen <- predict(Model_preprun, Testing_Data, type = "class")
table_mat <- table(Testing_Data$Decision, predict_unseen)
preprun_accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Preprruning  Accuracy for test', preprun_accuracy))
#Postpruning
# Prune the Preprunning model based on the optimal cp value

Model_pruned <- prune(Model_preprun, cp = 0.0084 )
# Compute the accuracy of the pruned tree

predict_unseen <- predict(Model_pruned, Testing_Data, type = "class")
table_mat <- table(Testing_Data$Decision, predict_unseen)
pruned_accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Postpruning  Accuracy for test', pruned_accuracy))
data.frame(base_accuracy, preprun_accuracy, pruned_accuracy)
toc()


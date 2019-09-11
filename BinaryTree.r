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
predict_unseen <-predict(Model, Testing_Data, type = 'class')## Using training model to predict test set
table_mat <- table(Testing_Data$Decision, predict_unseen)## Create a table to count how many passengers are classified as acceptance & rejection and passed away compare to the correct classification
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)## To calculate the accuracy of the model.
print(paste('Accuracy for Model', accuracy_Test))## for printing the accuracy of the model.
toc()## ends the time for computation
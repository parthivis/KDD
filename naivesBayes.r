options(warn =-1)
library(caret) ## library required to built confusionMatrix
library(tictoc) ## for calculating the time the algorithm is taking to build a model
library(e1071) ##needed for naive bayes
tic("time taken") ## starts the time for computation
dataset <- read.csv("C:\\Users\\Shreya'\\Downloads\\data wirh removed missing values 3 (1).csv",header = TRUE,sep = "," , na.strings = "?") ##Reading the data
partition = sample(nrow(dataset),0.7*nrow(dataset)) ##sampling dataset
training = dataset[partition,] ## partitioning the data into training set
testing_Datset = dataset[-partition,] ##partitioning the data into testing set
 a = training[,-12]   ## x value of the model
 y = training$Decision ##y value of the model
 builtmodel = train(a,y,'nb',trControl = trainControl(method = 'cv',number = 10)) ##building the model
Predict <- predict(builtmodel,newdata = testing_Datset) ##Evaluation of model
save = confusionMatrix(Predict,testing_Datset$Decision) ##To find correct as well as incorrect instances and to display accuracy achieved.
print(save) ##Printing the confusionMatrix.
toc() ## ends the time for computation.
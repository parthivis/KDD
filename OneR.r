options(warn =-1)
library(tictoc)## for calculating the time the algorithm is taking to build a model
tic("time taken")## starts the time for computation
library(OneR)## invoking the library for building model using OneR algorithm
AdmitData <- read.csv(file = "C:\\Users\\Pranjal\\Desktop\\Summer2019\\Project\\datafinal.csv",header = TRUE,sep = "," , na.strings = "?") ##Reading the data
set.seed(12)##for reproducibility
data1 <- sort(sample(nrow(AdmitData), nrow(AdmitData) * 0.8)) ## Random sampling of the data using sample() 
## and then splitting the data in to the ration of 80 % training data and 20% testing data 
training <- AdmitData[data1,]
dim(training)## Training data :55480    13
testing <-AdmitData[-data1,]
dim(testing)##Testing data : 13870    13 
model_train <- OneR(training, VERBOSE = TRUE) ## building a training model using OneR algorithm.
summary(model_train)
model_train[["correct_instances"]]## Correctly classified instances : 42252
model_train[["total_instances"]]## Total number of training intances :55480
prediction <- predict(model_train, testing) ## Using training model to predict test set
eval_model(prediction, testing) ## Evaluating the model performance on test set
toc()## ends the time for computation

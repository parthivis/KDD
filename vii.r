
## Exercise 8.4.8
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019



#8. In the lab, a classiﬁcation tree was applied to the 
#Carseats data set after converting Sales into a qualitative 
#response variable. Now we will seek to predict Sales using 
#regression trees and related approaches, treating the response as a quantitative variable.

#(a) Split the data set into a training set and a test set.

library(ISLR)
attach(Carseats)
set.seed(1)
data1 <- sample(nrow(Carseats), nrow(Carseats) * 0.7)
Training_data <- Carseats[data1,]
Testing_data <- Carseats[-data1,]

#(b) Fit a regression tree to the training set. Plot the tree, 
#and interpret the results. What test MSE do you obtain?

library (tree)
Carseats_Tree <- tree(Sales ~ ., data = Training_data)
summary(Carseats_Tree)
plot(Carseats_Tree)
text(Carseats_Tree,pretty=0)
yhat <-predict (Carseats_Tree, newdata = Testing_data)
test_MSE <- mean((Testing_data$Sales - yhat )^2)
test_MSE
## The test MSE is 4.208383 . The square root of the MSE is therefore around 2.0514,
# indicating that this model leads to test predictions that are within around $20514 of the true median
# home value for the suburb.

#(c) Use cross-validation in order to determine the optimal 
#level of tree complexity. Does pruning the tree improve the test MSE?

cv.Carseats <- cv.tree(Carseats_Tree)
plot(cv.Carseats$size ,cv.Carseats$dev ,type="b")
prune.Carseats <- prune.tree(Carseats_Tree ,best=9)
plot(prune.Carseats)
text(prune.Carseats,pretty=0)
prune.predict <- predict(prune.Carseats,Testing_data)
test_MSE_Pruned <- mean((prune.predict - Testing_data$Sales)^2)
test_MSE_Pruned
## Pruning of the tree has increased the test MSE to 4.469155 .

#(d) Use the bagging approach in order to analyze this data. 
#What test MSE do you obtain? Use the importance() function 
#to determine which variables are most important.

library(randomForest)
set.seed(1)
bag.Carseats <- randomForest(Sales~.,data <- Training_data, mtry=10,importance=TRUE) 
bag.Carseats
yhat.bag = predict (bag.Carseats,newdata= Testing_data)
Bag_MSE <- mean((yhat.bag -Testing_data$Sales)^2)
Bag_MSE
## The test MSE is 2.574782.
importance (bag.Carseats)
## Price and ShelveLoc are the two very important variables.

#(e) Use random forests to analyze this data. 
#What test MSE do you obtain? Use the importance() function 
#to determine which variables are most important. 
#Describe the eﬀect of m, the number of variables 
#considered at each split, on the error rate obtained.

set.seed(1)
rf.Carseats <- randomForest(Sales~.,data = Training_data, mtry=10,importance=TRUE) 
yhat.rf <- predict(rf.Carseats ,newdata  = Testing_data)
mean((yhat.rf - Testing_data$Sales)^2)
## The test MSE is 2.573252.
importance (rf.Carseats)
## Price and ShelveLoc are the two very important variables.
## When m predictors is chosen as split candidates from the full set of p predictors, it effects the error rate . 
## If  a random forest is built using m = p , then it indicates bagging . When m = square root of p gives small improvment 
## over m = p.
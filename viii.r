## Exercise 9.7.5
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019

# 5. We have seen that we can ﬁt an SVM with a non-linear kernel 
# in order to perform classiﬁcation using a non-linear decision 
# boundary. We will now see that we can also obtain a non-linear
# decision boundary by performing logistic regression using non-linear
# transformations of the features.

# (a) Generate a data set with n = 500 and p = 2, such that the observations 
# belong to two classes with a quadratic decision boundary between them.
# For instance, you can do this as follows:

 x1=runif (500) -0.5 
 x2=runif (500) -0.5 
 y=1*(x1^2-x2^2 > 0)

# (b) Plot the observations, colored according to their class labels. 
# Your plot should display X1 on the x-axis, and X2 on the yaxis.

plot(x1,x2 , xlab = "X1", ylab = "X2", col = (4-y), pch = (3-y))

# (c) Fit a logistic regression model to the data, using X1 and X2 as predictors.

logistic.fit = glm(y ~ x1 + x2 , family = "binomial")
summary(logistic.fit)

# (d) Apply this model to the training data in order to obtain a predicted class label 
# for each training observation. Plot the observations, colored according to the 
# predicted class labels. The decision boundary should be linear.

data =data.frame(x1 = x1, x2 = x2 , y = y)
ypred = predict(logistic.fit , data , type = "response")
logi.pred = rep(0,500)
logi.pred[ypred>.47] = 1
plot(data[logi.pred == 1,]$x1 , data[logi.pred == 1,]$x2 , col = (4-1),pch = (3-1),xlab = "X1" , ylab = "X2")
points(data[logi.pred == 0,]$x1, data[logi.pred == 0,]$x2, col = (4-0) , pch = (3-0))
## We got a linear decision boundary.

# (e) Now ﬁt a logistic regression model to the data using non-linear 
# functions of X1 and X2 as predictors (e.g. X2 1, X1×X2, log(X2),and so forth)

non_linear_logistic = glm(y~ poly(x1,2) + poly(x2,2)  + I(x1 * x2), family = "binomial")
summary(non_linear_logistic)

# (f) Apply this model to the training data in order to obtain a predicted class label
# for each training observation. Plot the observations, colored according to the predicted
# class labels. The decision boundary should be obviously non-linear.
# If it is not, then repeat (a)-(e) until you come up with an example
# in which the predicted class labels are obviously non-linear.

ypred = predict(non_linear_logistic ,data , type = "response")
nonlogi.pred = rep(0,500)
nonlogi.pred[ypred >.47] = 1
plot(data[nonlogi.pred == 1,]$x1 , data[nonlogi.pred == 1,]$x2 , col = (4-1),pch = (3-1),xlab = "X1" , ylab = "X2")
points(data[nonlogi.pred == 0,]$x1, data[nonlogi.pred == 0,]$x2, col = (4-0) , pch = (3-0))
## The decision boundary is same as true decision boundary .

# (g) Fit a support vector classiﬁer to the data with X1 and X2 as predictors.
# Obtain a class prediction for each training observation. 
# Plot the observations, colored according to the predicted class labels.

data$y= as.factor(data$y)
library(e1071)
svmfit=svm(y~ x1 +x2 , data=data  , kernel ="linear" , cost = .02)
svmfit.pred = predict(svmfit,data )
plot(data[svmfit.pred == 0,]$x1 , data[svmfit.pred == 0,]$x2 , col = (4-0),pch = (3-0),xlab = "X1" , ylab = "X2")
points(data[svmfit.pred == 1,]$x1, data[svmfit.pred == 1,]$x2, col = (4-1) , pch = (3-1))

## Using this support vector classifier with low cost ,it classifies all points to a single class.

# (h) Fit a SVM using a non-linear kernel to the data. 
# Obtain a class prediction for each training observation.
# Plot the observations, colored according to the predicted class labels.

data$y=as.factor(data$y)
svmfit.non_linear = svm(y~ x1 +x2 , data=data, kernel="radial", gamma=1)
svmfit.non_linear.pred <- predict(svmfit.non_linear, data )
plot(data[svmfit.non_linear.pred == 0,]$x1 , data[svmfit.non_linear.pred== 0,]$x2 , col = (4-0),pch = (3-0),xlab = "X1" , ylab = "X2")
points(data[svmfit.non_linear.pred == 1,]$x1, data[svmfit.non_linear.pred == 1,]$x2, col = (4-1) , pch = (3-1))
## The decision boundary is same as true decision boundary .

# (i) Comment on your results.

## We conclude that with both the SVM's such as with non-linear kernel and with logistic regression
## with interaction terms are equally 
## good for finding non-linear decision boundaries. 
## In addition to this ,SVM  without any 
## interaction term using both linear kernel 
## and logistic regression are very bad for finding non-linear 
## decision boundaries. Also , when using SVM 
## for logistic regression , tuning is done manually when finding right interaction terms 
##, and when using SVM we tune gamma.
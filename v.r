## Exercise 6.8.10
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019

#10. We have seen that as the number of features 
#used in a model increases, the training error will necessarily 
#decrease, but the test error may not. We will now explore this in a simulated data set.
#(a) Generate a data set with p = 20 features,n =1 ,000 observations, 
#and an associated quantitative response vector generated according to the 
#model Y = Xβ+ , where β has some elements that are exactly equal to zero.

set.seed(1)
x <- matrix(rnorm(1000*20),1000,20)
b <-rnorm(20)
b[3] <- 0
b[5] <- 0
b[7] <- 0
b[9] <- 0
b[11] <- 0
error <- rnorm(1000)
y <- x %*% b + error

#b) Split your data set into a training set 
#containing 100 observations and a test set containing 900 observations.

training <- sample(seq(1000),100 ,replace = FALSE)
testing <- (-training)
x.training <- x[training, ]
x.testing <- x[testing, ]
y.training <- y[training]
y.testing <- y[testing] 

#c) Perform best subset selection on the training set, 
#and plot the training set MSE associated with the best model of each size.

library(leaps)
data.training <- data.frame(y = y.training ,x = x.training)
regfit.full <- regsubsets(y ~ . ,data = data.training , nvmax = 20)
training.matrix <- model.matrix(y ~. ,data = data.training, nvmax =20)
val.errors<-rep(NA,20)
for(i in 1:20){
	coefi<-coef(regfit.full ,id=i)
	pred<-training.matrix[,names(coefi)]%*%coefi 
	val.errors[i]<-mean((pred - y.training)^2)
	} 
plot(val.errors, xlab = "Number of predictors", ylab = "Training MSE", pch = 19,type = "b",col="pink")	

#(d) Plot the test set MSE associated with the best model of each size.

data.testing <- data.frame(y = y.testing ,x = x.testing)
testing.matrix <- model.matrix(y ~. ,data = data.testing, nvmax =20)
val.errors<-rep(NA,20)
for(i in 1:20){
	coefi<-coef(regfit.full ,id=i)
	pred<-testing.matrix[,names(coefi)]%*%coefi 
	val.errors[i]<-mean((pred - y.testing)^2)
	} 
plot(val.errors, xlab ="Number of predictors", ylab = "Test MSE", pch = 19,type = "b",col="blue")

#(e) For which model size does the test set MSE take on its minimum value?
# Comment on your results. If it takes on its minimum value for a model containing
# only an intercept or a model containing all of the features, 
# then play around with the way that you are generating the data in 
#(a) until you come up with a scenario in which the test set MSE is minimized for an intermediate model size.

which.min(val.errors)
## For the model having 18 variables the test set MSE is minimum for the model containing all of the features.

#(f) How does the model at which the test set
# MSE is minimized compare to the true model used
# to generate the data? Comment on the coeﬃcient values.

coef(regfit.full, which.min(val.errors))
## In the true model , we zeroed certain parameters.
## The best subset model with test MSE minimum built is able to identify those parameters
## and removed them from the model.

#(g) Create a plot displaying '
#p j=1(βj − ˆ βr j)2 for a range of values of r, whereˆ βr j is the jth 
#coeﬃcient estimate for the best model containing r coeﬃcients. 
#Comment on what you observe. How does this compare to the test MSE plot from (d)?

val.errors<-rep(NA,20)
c = colnames(x, do.NULL = FALSE, prefix = "x.")
for(i in 1:20){
	coefi<-coef(regfit.full ,id=i)
	val.errors[i]<- sqrt(sum((b[c %in% names(coefi)] - coefi[names(coefi) %in% c ])^2) + sum(b[!(c %in% names(coefi))])^2)
	} 
plot(val.errors, xlab ="Number of predictors", ylab = "Mean Squared error for estimates coefficients", pch = 19,type = "b",col="blue")
## The error is minimized for 2 variables 
##and decreases for variables from 14-20.Minimum test MSE is 14 .
##The test MSE is minimum for 14 variable model.
##So ,it does not necessory mean a lower test MSE for  
##better fit of true coefficients .Therefore ,it need not be the best model to predict.

## Exercise 4.7.11
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019


#11. In this problem, you will develop a model to 
#predict whether a given car gets high or low gas 
#mileage based on the Auto data set.

#(a) Create a binary variable, mpg01, 
#that contains a 1 if mpg contains a 
#value above its median, and a 0 if mpg 
#contains a value below its median. 
#You can compute the median using the median() function. 
#Note you may ﬁnd it helpful to use the data.frame() function 
#to create a single data set containing both mpg01 
#and the other Auto variables.

Auto <- read.csv(file = "C:\\Users\\Pranjal\\Desktop\\Summer2019\\Auto.csv",header = TRUE,sep = "," , na.strings = "?")
Auto <- na.omit(Auto)
nrow(Auto)
attach(Auto)
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)
Autos <- data.frame(Auto,mpg01)
summary(Autos)

#(b) Explore the data graphically in order to 
#investigate the association between mpg01 and 
#the other features. Which of the other features
# seem most likely to be useful in predicting mpg01?
# Scatterplots and boxplots may be useful tools to answer this question. Describe your ﬁndings.

pairs(Autos)
par(mfrow = c(2,2))
boxplot(mpg01~cylinders, data = Autos , xlab ="mpg01 predicting" , ylab ="cylinders")
boxplot(mpg01~displacement, data = Autos , xlab ="mpg01 predicting" , ylab ="displacement")
boxplot(mpg01~horsepower, data = Autos , xlab ="mpg01 predicting" , ylab ="horsepower")
boxplot(mpg01~weight, data = Autos , xlab ="mpg01 predicting" , ylab ="weight")
boxplot(mpg01~acceleration, data = Autos , xlab ="mpg01 predicting" , ylab ="acceleration")
## From the scatterplots and the boxplots we can see that
## displacement ,horsepower , weight are associated with mpg01.

#(c) Split the data into a training set and a test set.
set.seed(1)
data1 <- sort(sample(nrow(Autos), nrow(Autos) * 0.7))
training <- Autos[data1,]
dim(training)
testing <-Autos[-data1,]
dim(testing)

#(d) Perform LDA on the training data in order to 
#predict mpg01 using the variables that seemed most 
#associated with mpg01 in (b). What is the test error of the model obtained? 

library(MASS)
lda.fit <- lda(mpg01~displacement + horsepower + weight, data = training)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit,testing)
names(lda.pred)
lda.class <-lda.pred$class
table(lda.class,testing$mpg01)
mean(lda.class !=testing$mpg01)
##.1271186 or 12.7% is the test error of the model obtained

#(e) Perform QDA on the training data in order 
#to predict mpg01 using the variables that seemed 
#most associated with mpg01 in (b). What is 
#the test error of the model obtained?

qda.fit <- qda(mpg01~displacement + horsepower + weight, data = training)
qda.fit
qda.class <- predict(qda.fit,testing)$class
table(qda.class,testing$mpg01)
mean(qda.class !=testing$mpg01)
## test error is .1101695 or 11% .

#(f) Perform logistic regression on the training data 
#in order to predict mpg01 using the variables that 
#seemed most associated with mpg01 in (b). 
#What is the test error of the model obtained?

glm.fit <- glm(mpg01~displacement + horsepower + weight ,data = training ,family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,testing ,type = "response")
glm.probs[1:10]
glm.pred <- rep(0,length(glm.probs))
glm.pred[glm.probs>.5]= 1
mean(glm.pred!= testing$mpg01)
##  0.09322034 or 9.32 % is the test error.

#(g) Perform KNN on the training data, with several values 
#of K, in order to predict mpg01. Use only the variables that 
#seemed most associated with mpg01 in (b). What 
#test errors do you obtain? Which value of K seems 
#to perform the best on this data set?

library(class)
train.X = cbind(training$displacement + training$horsepower + training$weight)
test.X = cbind(testing$displacement + testing$horsepower + testing$weight)
train.mpg01 = training$mpg01
set.seed(1)
knn.pred=knn(train.X,test.X,training$mpg01,k=1)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1949153 or 19.4 % test error 

knn.pred=knn(train.X,test.X,training$mpg01,k=5)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1355932 or 13.5 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=10)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1440678 or 14.4 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=15)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1355932 or 13.5 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=25)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1355932 or 13.5 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=35)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1355932 or 13.5 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=65)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1440678 or 14.4 % test error


knn.pred=knn(train.X,test.X,training$mpg01,k=100)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1355932 or 13.5 % test error

knn.pred=knn(train.X,test.X,training$mpg01,k=120)
table(knn.pred, testing$mpg01)
mean(knn.pred != testing$mpg01)
##[1] 0.1440678 or 14.4 % test error

## At k = 5,15,25,35,100  works best.
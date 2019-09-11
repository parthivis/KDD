
## Exercise 5.4.6
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019


#We continue to consider the use of a logistic regression model to predict the probability of default using income and balance on the Default data set. 
#In particular, we will now compute estimates for the standard errors of the income and balance logistic regression co- eﬃcients in two diﬀerent ways: 
#(1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the glm() function.
#Do not forget to set a random seed before beginning your analysis.

#a) Using the summary() and glm() functions, 
#determine the esti- mated standard errors for the coeﬃcients associated with income and balance in a multiple logistic regression model that uses both predictors.

options(digits = 4)
library(ISLR)
set.seed(1)
glm.fit <- glm(default~income+balance,data=Default,family = "binomial")
summary(glm.fit)

#(b)Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, 
#and that outputs the coeﬃcient estimates for income and balance in the multiple logistic regression model.

boot.fn <- function(data = Default, index) {
glm.fit <- glm(default~income+balance,data=Default,family = "binomial",subset = index)
return(coef(glm.fit))
}
#(c)Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic regression coeﬃcients for income and balance.

library(boot)
boot(Default,boot.fn,1000)

#(d)Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function.
##Standard errors calculated by glm function are 0.435, 4.99 *10^(-6),2.27*10^(-4) respectively.
##Standard errors calculated by bootstrap function are 0.434, 4.86 *10^(-6),2.29*10^(-4) respectively.
##On the basis of the observations, standard errors calculated using glm as well as bootstrap function are pretty close.





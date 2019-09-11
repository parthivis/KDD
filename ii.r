## Exercise 3.7.9
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019

#9. This question involves the use of multiple linear regression on the Auto data set.

# (a) Produce a scatterplot matrix which includes all of the variables in the data set.

Auto <- read.csv(file = "C:\\Users\\Parthivi\\Desktop\\Summer2019\\Auto.csv",header = TRUE,sep = "," , na.strings = "?")
Auto <- na.omit(Auto)
nrow(Auto)
attach(Auto)
name <-as.factor(name)
pairs(Auto)


# (b) Compute the matrix of correlations between the variables using the function cor(). 
#You will need to exclude the name variable, which is qualitative.

cor(subset(Auto, select=-name))


# (c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except
# name as the predictors. Use thesummary() function to print the results. Comment on the output.For instance:

LmAuto = lm(mpg ~.-name , Auto)
summary(LmAuto)

# i. Is there a relationship between the predictors and the response?

##  Yes there is a relationship between predictors and response by testing the null hypothesis . Recalling the null hypothesis
## which states that estimates or regression coefficients is suﬃciently far from zero 
## that we can be conﬁdent that coefficients is non-zero. And here we observe using F statistic , that we have a small
## p value . Therefore we infer that there is a relationhip between predictors and response. 

# ii. Which predictors appear to have a statistically signiﬁcant relationship to the response? 

## displacement , year , origin , weight are statistically significant.

# iii. What does the coeﬃcient for the year variable suggest? 

##  The regression coefficient for year is positive which is  0.750773, which indicates that  
##  mpg increases by the coefficient for every one year.
##  Cars will become more fuel efficient by approx 1 mg per year for every year.


# (d) Use the plot() function to produce diagnostic plots of the linear regression ﬁt. 
#Comment on any problems you see with the ﬁt. Do the residual plots suggest any unusually large outliers? 
#Does the leverage plot identify any observations with unusually high leverage?

par(mfrow=c(2,2))
plot(LmAuto)
## From the graph residuals versus fitted values, indicates that the fit is not accurate . 
## We can see a noticable curve and a non-linear relation between predictor and response.
## From the standardized residuals versus leverage  plot , indicates fewer oultiers and a few high leverage points.


#(e) Use the * and : symbols to ﬁt linear regression models with interaction eﬀects.
# Do any interactions appear to be statistically signiﬁcant?

LmAutompg = lm(mpg~cylinders*displacement+displacement*weight + acceleration*year + horsepower:origin + acceleration:horsepower + horsepower:displacement)
summary(LmAutompg)
## Only the variables displacement and weight interactions  
## acceleration and year, acceleration and horsepower are  statistically significant but 
## the same is not true for interactiion between cylinders and displacement is not.


# (f) Try a few diﬀerent transformations of the variables, such as log(X), √X, X2. Comment on your ﬁndings.

AutoF = lm(mpg~ . + log(displacement) + sqrt(acceleration) + I(cylinders ^ 2) + sqrt(year) +log(horsepower) -name ,data = Auto)
summary(AutoF)
par(mfrow=c(2,2))
plot(AutoF)
## log(horsepower), sqrt(year) and log(displacement) are statistically significant but not others.

AutoF2 = lm(mpg~ . + log(acceleration) + sqrt(displacement) + I(cylinders ^ 2) + sqrt(year) +log(horsepower) -name ,data = Auto)
summary(AutoF2)
par(mfrow=c(2,2))
plot(AutoF2)
## sqrt(displacement) , log(horsepower), sqrt(year) are statistically significant but not others.

AutoF3 = lm(mpg~ . + log(origin) + sqrt(cylinders) + I(horsepower ^ 2) + sqrt(year) +log(horsepower) -name ,data = Auto)
summary(AutoF3)
par(mfrow=c(2,2))
plot(AutoF3)
## log(horsepower), sqrt(year) are statistically significant but not others.

















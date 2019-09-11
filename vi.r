## Exercise 7.9.7
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019

#7. The Wage data set contains a number of other features not explored in this chapter, 
#such as marital status (maritl), job class (jobclass), and others. Explore the
#relationships between some of these other predictors and wage, and use 
#non-linear fitting techniques in order to fit fexible models to the data. 
#Create plots of the results obtained, and write a summary of your findings

options(warn = -1)
library(ISLR)
attach(Wage)
summary(Wage)
summary(Wage$maritl)
summary(Wage$jobclass)
summary(Wage$race)
summary(Wage$health)

par(mar = rep(2, 4))
plot(Wage$maritl,Wage$wage)
## From the above plot we conclude that married couple earns more money than Never Married, Windowed, Separated , Divorced.

par(mar = rep(2, 4))
plot(Wage$jobclass,Wage$wage)
## From the above plot we conclude that people who has Information earns more money.

par(mar = rep(2, 4))
plot(Wage$race,Wage$wage)
## From the above plot we conclude that people = Asians earns more money.

par(mar = rep(2, 4))
plot(Wage$health,Wage$wage)
## From the above plot we conclude that people with Very Good health earns more money.

library(gam)
gam.m1 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education, data = Wage)
gam.m2 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education + maritl, data = Wage)
gam.m3 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education + jobclass, data = Wage)
gam.m4 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education + race, data = Wage)
gam.m5 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education + health, data = Wage)
gam.m6 <- gam(wage~lo(year,span = 0.7) + s(age,5) + education + maritl + jobclass + race + health ,data = Wage)
anova(gam.m1,gam.m2,gam.m3,gam.m4,gam.m5,gam.m6)
par(mar = rep(2, 4))
plot(gam.m6 , se = TRUE, col = "blue")
summary(gam.m6)
## From the plots we got to know that wage increases from the year 2005 and wage increases at the age 30 but decreases at the age of 45 
## In the above models year,age , and education are qualitative variables with other features maritl, jobclass, race , health.
## We used non-linear fitting technique gam() function to fit fexible models to the data.We used anova test to measure the goodness of fit of each model . 
## We determined that the model 2 i.e (gam.m2) and model 6(gam.m6) are most significant models.
## Using anova test for nonparametric effects we determined that age is significant for our model.
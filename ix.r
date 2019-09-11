## Exercise 10.7.10
## Parthivi Shrivastava,Shreya Kulkarni
## Summer 2019

# 10. In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.

# (a) Generate a simulated data set with 20 observations in each of three classes 
# (i.e. 60 observations total), and 50 variables. Hint: There are a number of 
# functions in R that you can use to generate data. One example is the rnorm() function;
# runif() is another option. Be sure to add a mean shift to the observations in 
# each class so that there are three distinct classes.

set.seed(2)
x <- matrix(rnorm(20*3*50,mean = 0 , sd = .1), ncol = 50)
x [1:20,2] <- 1
x [21:40,1] <- 2
x [21:40, 2] <- 2
x[41:60,1] <- 1
labels <- c(rep(1,20),rep(2,20), rep(3,20))

# (b) Perform PCA on the 60 observations and plot the ﬁrst two principal component score vectors. 
# Use a diﬀerent color to indicate the observations in each of the three classes. 
# If the three classes appear separated in this plot, then continue on to part (c). 
# If not, then return to part (a) and modify the simulation so that there is greater separation 
# between the three classes. Do not continue to part (c) until the three classes show at least 
# some separation in the ﬁrst two principal component score vectors.

pca_x <- prcomp(x)
plot(pca_x$x[,1:2],col = 1:3 ,xlab = "P1", ylab = "P2",pch = 19)

# (c) Perform K-means clustering of the observations with K = 3. How well do the clusters that 
# you obtained in K-means clustering compare to the true class labels? 
# Hint: You can use the table() function in R to compare the true class labels to the class labels 
# obtained by clustering. Be careful how you interpret the results: K-means clustering will 
# arbitrarily number the clusters, so you cannot simply check whether the true class labels and clustering labels are the same.

km.out.1 <- kmeans(x,3,nstart =20)
table(labels,km.out.1$cluster)
## We have 3 classes in the dataset with 20 observation in each of the classes and 
## in all the 3 classes we obtained perfect clusters from k-means clustering.

# (d) Perform K-means clustering with K = 2. Describe your results.

km.out.2 <- kmeans(x,2,nstart = 20)
table(labels,km.out.2$cluster)
## All original 3 classes are now classified with into 2 clusters.
## We obtained that one out of 3 clusters is now cubbed into one out of two clusters.

# (e) Now perform K-means clustering with K = 4, and describe your results.

km.out.3 <- kmeans(x,4,nstart = 20)
table(labels,km.out.3$cluster)
## All original 3 classes are now classified with into 4 clusters. 
## The first cluster is splitted in to 2 clusters ( 1st & 4th cluster).

# (f) Now perform K-means clustering with K = 3 on the ﬁrst two principal component score vectors, 
# rather than on the raw data. That is, perform K-means clustering on the 60 × 2 matrix of which the 
# ﬁrst column is the ﬁrst principal component score vector, and the second column is the 
# second principal component score vector. Comment on the results.

km.pca_x <- kmeans(pca_x$x[,1:2], 3 ,nstart = 20)
table(labels ,km.pca_x$cluster)
## All 3 original classes are perfectly classified into 3 clusters.

# (g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each variable 
# to have standard deviation one. How do these results compare to those obtained in (b)? Explain.

km.scale <- kmeans(scale(x),3 ,nstart = 20)
table(labels , km.scale$cluster)
## We observed that the clusters are not clustered perfectly . We got poor results using the scale() 
## because scaling affects the distance between the observations.



install.packages("scatterplot3d")
library(Amelia)
x<-read.csv("/Users/vikaschhillar/Downloads/iris_with_missing_data.csv")
y<-read.csv("/Users/vikaschhillar/Downloads/iris-imp5.csv")
summary(x)
aggregate(x)
mean(x)
plot(Sepal.Length~Sepal.Width,data=x)
mean(x)
mean.length=mean(x$Sepal.Length,na.rm = T)
mean.length
abline(h=mean.length) # no relationship between length and width 
# use lm to fit a regression line
model1=lm(Sepal.Length~Sepal.Width,data=x )
abline(model1,col="red")
plot(model1)
termplot(model1)
summary(model1)

#static 3d scatterplot 
library(scatterplot3d)
scatterplot3d(x[1:3])

#coloring, vertical line and regression plane
s3d<-scatterplot3d(x[1:3],
                   pch=16,
                   highlight.3d = TRUE,
                   type = "h",
                   main = "3d scatteplot")

plane<-lm(x$Petal.Length~x$Sepal.Length+x$Sepal.Width)
s3d$plane3d(plane)
install.packages("rgl")
library("rgl")

data(iris)
set.seed(250)
par(cex.lab = 1.5, cex.main = 1.2)
Data <- scale(iris[,-5]) # notice I am scaling the vectors)
clustergram(Data, k.range = 2:8, sepal.length = 0.004) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.


x
plot(x$Petal.Length, x$Petal.Width, main="Edgar Anderson's Iris Data")
plot(x$Petal.Length, x$Petal.Width, pch=c(23,24,25)[unclass(x$Species)], main="Edgar Anderson's Iris Data")
plot(x$Petal.Length, x$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(x$Species)], main="Edgar Anderson's Iris Data")
pairs(x[1:5], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(x$Species)])

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
pairs(x[1:5], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red","green3","blue")[unclass(x$Species)], upper.panel=panel.pearson)

pairs(x[1:5], main = "Anderson's Iris Data -- 3 species", pch = 21, bg = c("red", "green3", "blue")[unclass(x$Species)], lower.panel=NULL, labels=c("SL","SW","PL","PW","SC"), font.labels=2, cex.labels=4.5)


data("iris")
sumarry(x)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
set.seed(20)
set.seed(20)
irisCluster <- kmeans(x[, 3:4], 3, nstart = 20)
irisCluster

AmeliaView()
install.packages("tcltk")

y<-read.csv("/Users/vikaschhillar/Downloads/iris-imp5.csv")



####KNN Algorithm(easy to work with numerical)
str(y)
table(y$Species)
head(y)
x
set.seed(9850)  #Random number generator
gp<-runif(5) # 5 random numbers between 0 and 5
gp
gp<-runif(nrow(y))
gp
y<-y[order(gp),] # take order of gp
head(y)
str(y)
summary(y[,c(1,2,3,4)]) #sepal.length varies from 4.3 to 7.9, 

# normalize-it takes each value and subtracts the min and divide bt max-min and will rescale all features)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
normalize(c(1,2,3,4,5))

#apply the function
Iris_n<-as.data.frame(lapply(y[,c(1,2,3,4)],normalize))
str(Iris_n)
summary(Iris_n) # the min is 0 and max is 1

#create a training dataset amnd separate test dataset which will help us acess our model
# training dataset is used to learn a pattern and test will serve a test how well our model predict
# 10 % for testing 
str(y)
Iris_train<-Iris_n[1:129,]
Iris_test<-Iris_n[130:150,]

# isolate the species
Iris_train_target<-y[1:129,5]
Iris_test_target<-y[130:150,5]

#KNN is located in the class package
require(class)
m1<-knn(train=Iris_train, test=Iris_test,cl=Iris_train_target, k=13)
m1 #m1 has stored prediction of classification in test dataframe 

#confusion matrix
table(Iris_test_target,m1)   #prediction on x axis and actual values on y axis





head(x)
summary(x$Sepal.Length)
boxplot(x$Sepal.Length,x$Sepal.Width,col="blue")
title(main="Nominal Major Currencies Dollar Index",sub="Mar 1973 = 100.00", ylab="US$ vs major currencies")
)

boxplot(x$Sepal.Length,x$Sepal.Width)

boxplot(mpg~cyl,data=x, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


### k means clustering
y
iris.features=y
iris.features$class<-NULL
iris.features(c(1,2,3,4))



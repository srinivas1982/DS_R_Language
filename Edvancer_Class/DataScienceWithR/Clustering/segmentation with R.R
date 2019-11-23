## ----echo=FALSE,warning=FALSE,message=FALSE------------------------------
library(knitr)
library(dplyr)

## ----echo=FALSE----------------------------------------------------------
library(xtable)
Age=c(32,30,34,20,22,24)
Income=c(150,156,152,148,156,150)
g1=as.factor(c(2,3,2,1,3,1))
d=data.frame(Age,Income)
tbl_df(d)

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
library(ggplot2)
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()


## ----echo=FALSE----------------------------------------------------------

d$Income=d$Income/10
tbl_df(d)

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()

## ----echo=FALSE----------------------------------------------------------
d$Age=(d$Age-mean(d$Age))/sd(d$Age)
d$Income=(d$Income-mean(d$Income))/sd(d$Income)
tbl_df(d)

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()

## ----echo=FALSE----------------------------------------------------------
k=100-100*c(0.00,0.30,0.45,0.84,0.89,0.92,0.94,0.95,0.96,0.96)
d=data.frame(Rsq=c(0.00,0.30,0.45,0.84,0.89,0.92,0.94,0.95,0.96,0.96),SSW=k, K=as.factor(c(1:10)))
p=ggplot(d,aes(x=K,y=SSW,group=1))
p+geom_point(color="red",size=5)+geom_path(color="blue")

## ------------------------------------------------------------------------
data(mtcars)
cars=mtcars

## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
groups.3=cutree(cars.hclust,3)

groups.3
table(groups.3)
rownames(cars)[groups.3==1]

## ------------------------------------------------------------------------
n = 100
g = 6 
set.seed(g)
d <- data.frame(
  x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
  y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d,col="mediumturquoise",pch=16,
     xlab="Arrival Time Deviations", 
     ylab="Departure time Deviations",
     main="Scatter Plot: Delays from Schedule in Minutes ")

## ------------------------------------------------------------------------
mydata <- d

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)

## ------------------------------------------------------------------------
require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

## ------------------------------------------------------------------------
fit <- kmeans(mydata,4 ) 

d$cluster=fit$cluster

library(ggplot2)

ggplot(d,aes(x,y,color=as.factor(cluster)))+geom_point()

## ------------------------------------------------------------------------
setwd("/media/dell/9941-19EE/Edvancer_Class/Data/Data")
wine=read.csv("winequality-red.csv",sep=";")
wine=wine %>%
  select(pH,sulphates,alcohol,total.sulfur.dioxide)

## ------------------------------------------------------------------------
md=function(x){
  return((x-mean(x))/sd(x))
}
wine_std=wine %>%
  mutate(pH=md(pH),
         sulphates=md(sulphates),
         alcohol=md(alcohol),
         total.sulfur.dioxide=md(total.sulfur.dioxide))

require(vegan)
fit <- cascadeKM(wine_std, 1, 10, iter = 100)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

## ------------------------------------------------------------------------
fit <- kmeans(wine_std,5) 

wine_std$cluster=fit$cluster

library(ggplot2)

#pair wise profiling plots
ggplot(wine_std,aes(pH,alcohol,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(pH,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(pH,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(alcohol,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(alcohol,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(sulphates,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()


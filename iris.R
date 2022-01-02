#1
dev.off() # 초기화
par(mar=c(7,5,1,1))
boxplot(iris,las=2)

#2
dev.off()
irisVersi <- subset(iris, Species == "versicolor")
irisSeto <- subset(iris, Species == "setosa")
irisVirgin <- subset(iris, Species == "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVersi[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(irisSeto[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(irisVirgin[,1:4], main="Virginica",ylim = c(0,8),las=2)

#3
dev.off()
# install.packages("beanplot",dependencies = TRUE)
library(beanplot)
xiris <- iris
xiris$Species <- NULL
beanplot(xiris, main = "Iris flowers",col=c('#ff8080','#0000FF','#0000FF','#FF00FF'), border = "#000000")

#4
dev.off()
pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))

#5
dev.off()
library(MASS)
parcoord(iris[,1:4], col=iris[,5],var.label=TRUE,oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))




library(readxl)
require(psych)
library(readr)
#years <- read_excel("~/AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data/year.xlsx", col_names = FALSE)
#Year=data.frame(years)

#year=Year[,1] 

############################################################
#CLUSTER ANALYSIS (CA)
###############################################################
setwd("~/AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data")
Data1 <- read_csv("Data1.csv")
#Data1 <- read_csv("Data1.csv")
#Data1 <- read_csv("AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data/Data1.csv")
#Data1 <- read_excel("~/AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data/Data1.xlsx")
Data=Data1[,-1]
View(Data1)
#yy.scaled=scale(Data)



####### performing the clustering############
#yy.clus=hclust(dist(t(yy.scaled)),method='ward.D')

#####plotting the tree#################
#plot(yy.clus,hang=-2)


data2 <- scale(Data) ## standardizing the variables
data3<- dist(data2) # distance computation with year as observation
 
data4 <- dist(t(data2)) # distance computation with year as variables

ward = hclust(data4,method='ward.D2')
single= hclust(data4,method='single')
average = hclust(data4,method='average')


## Plotting the dendogram
par(mfrow = c(1 ,3))
plot(ward, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(ward, k =4, border = 'red') ## selecting three clusters
plot(single, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(single, k =4, border = 'red') ## selecting three clusters
plot(average, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(average, k =4, border = 'red') ## selecting three clusters

## cutting the trees to see the groups selected above
cutree (ward,4)
cutree (single,4)
cutree (average,4)



############################################################
#PRINCIPAL COMPONENT ANALYSIS (PCA)
###############################################################

pc1 <- principal(data2, nfactors = 3, rotate = "none")

pc1$loadings
pc1 # the full model to see the loadings, Eigenvalues  (SS loadings) and variance accounted for
pc1$scores




## screeplot
#par(mfrow = c(1,2))
pve <- 100* pc1$values/sum(pc1$values) 
plot(pc1$values, type ="o" , ylab ="Eigenvalues " , xlab ="Principal Component" ,
     col ="blue", main = 'screeplot')
plot (cumsum(pve) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')


pve1 <- 100* pc2$values/sum(pc2$values) 
plot (cumsum(pve1) , type ="o" , ylab ="Cumulative PVE " , xlab ="Principal Component " , col ="brown3 ",
      main = 'Percentage Variance Explained (PVE) plot')



## Rotated PCA after extracting four components
pc2 <- principal(data2, nfactors =3, rotate = "varimax") 
pc2$scores


scores<- as.data.frame(pc2$scores)  #scores after rotation
years=Data1$Year
#plotting some of the scores
par(mfrow=c(3,1))
plot(scores$RC3[1:120]~years[1:120],type ="o", xlab = 'year', ylab='RC3', main='Score plot of Rotated Component 3',col="tomato3")
abline(h=0.0, col="black")
plot(scores$RC3[121:241]~years[121:241],type ="o", xlab = 'year', ylab='RC3', main='Score plot of Rotated Component 3',col="tomato3")
abline(h=0.0, col="black")
plot(scores$RC3[242:612]~years[242:612],type ="o", xlab = 'year', ylab='RC3', main='Score plot of Component 3',col="tomato3")
abline(h=0.0, col="black")


plot(scores$RC2[1:120]~years[1:120],type ="o", xlab = 'year', ylab='RC2', main=' Score plot of Rotated Component 2',col="tomato3")
plot(scores$RC2[121:241]~years[121:241],type ="o", xlab = 'year', ylab='RC2', main='Score plot of Rotated Component 2',col="tomato3")
plot(scores$RC2[242:612]~years[242:612],type ="o", xlab = 'year', ylab='RC2', main=' Score plot of Rotated Component 2',col="tomato3")


plot(scores$RC1[1:120]~years[1:120],type ="o", xlab = 'year', ylab='RC1', main=' Score plot of Rotated Component 1',col="tomato3")
plot(scores$RC1[121:241]~years[121:241],type ="o", xlab = 'year', ylab='RC1', main='Score plot of Rotated Component 1',col="tomato3")
plot(scores$RC1[242:612]~years[242:612],type ="o", xlab = 'year', ylab='RC1', main='Score plot of Rotated Component 1',col="tomato3")

#plot(scores$RC2~year,type ="o", xlab = 'year', ylab='RC2', main='Rotated Component 2')

### scores plot for rotated
#RC11<- read_csv("AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data/xx.csv")

#library(ggplot2)
#RC1=data.frame(RC11)
#Rc1=RC1[241:612,]
#Rc2=RC1[1:240,]

#ggplot(Rc1, aes(x=Rc1$Year, y=Rc1$x)) +
#  geom_line()+geom_point()+ylab("RC1")+ggtitle("Scores plot of RC1 for rotated PCA ")

#ggplot(Rc2, aes(x=year, y=x)) +
  #geom_line()+geom_point()+ylab("RC1")+ggtitle("Scores plot of RC1 for rotated PCA")+xlim(1960,1980)


a=lm(scores$RC1~years)
summary(a)
pr.lm <- predict(a)

plot(years,scores$RC1 , "l", las=1, xlab="Time", ylab="Temperature")
lines(pr.lm~years, col="blue", lwd=2)




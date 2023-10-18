#Export file and mnipulated in excel
gfx_data
write.table(gfx_data, "gfx_data.csv")

-------------------------------------
  #Import the new excel file
  setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(ggplot2)
library(readxl)
library(psych)
install.packages("mlbench")
library(mlbench)
install.packages("mlbench")
library(DescTools)
install.packages("smbinning")
library(smbinning)
library(tidyr)
library(cluster)
install.packages("rattle.data")
library(rattle.data)

NFIP_Data <- read_excel("~/NFIP/Final_NFIP_Data.xlsx")
View(NFIP_Data)
str(NFIP_Data)
summary(NFIP_Data)
#Data fame converstion
tmp<-NFIP_Data
tmp$Policies_gfx_Class <- as.factor(tmp$Policies_gfx_Class)
tmp$Payment_gfx_Class <- as.factor(tmp$Payment_gfx_Class)
str(tmp)
summary(tmp)
tmp1<-tmp
summary(tmp1)
#If any remove all the NA from the table
vect<- c(1, 2, 3, NA)
is.na(vect)
tmp1$Payment_gfx_Class <- tmp1[complete.cases(tmp1),]
tmp1$Policies_gfx_Class <- tmp1[complete.cases(tmp1),]
NROW(tmp1)
summary(tmp1)
tmp1 = tmp1[complete.cases(tmp1),]
str(tmp1)

#Remove column
tmp_pol<- tmp1
#tmp_pol$Payment_gfx_Class<- NULL
tmp_pol$long<- NULL
tmp_pol$lat<- NULL
tmp_pol$group<- NULL
tmp_pol$order<- NULL
#tmp_pol$policies_gfx<- NULL
tmp_pol$Payment_gfx<- NULL
tmp_pol$Payment_gfx_Class<-NULL
tmp_pol$state<-NULL
tmp_pol$county<-NULL
str(tmp_pol)


cluster1<-tmp_pol
cluster1$Policies_gfx_Class <- as.numeric(cluster1$Policies_gfx_Class)
str(cluster1)
View(cluster1)

x <- cluster1[complete.cases(cluster1), ]
summary(x)

cluster.stand <- scale(x[-1])
k.means.fit <- kmeans(cluster.stand, 2) # k = 2

clusplot(cluster.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

head(x)
# set the seed
set.seed(1)
# cluster rows (learners) by k-means clustering, where k = 2
x.kmeans <- kmeans(x[ , 4:9], 2)
# assign cluster to the original data frame
x$cluster <- x.kmeans$cluster
head(x, 20)
summary(x$cluster)
options(width = 300)
frequency(x$cluster)
range(x$cluster)


#Cluster Valication

d <- x[ , 4:9]
set.seed(1)
res.kmeans <- kmeans(d, 2)
ddist <- dist(d)

sil <- silhouette(res.kmeans$cluster, ddist)
sil[1:5, ]
mean(sil[ , 3])



View(tmp_pol)
tmp_pol = tmp_pol[complete.cases(tmp_pol),]
summary(tmp_pol)
## same analysis, but now with clustering on all
## policy groups change the number of clusters to 7
# K-Means Clustering on Polices and Total loss
table(tmp_pol$Policies_gfx_Class)
newpolicy<-tmp_pol
newpolicy$Policies_gfx_Class<-NULL
str(newpolicy)
head(newpolicy)
set.seed(123456789)
kc<-kmeans(newpolicy[,1], centers=7, nstart=10)
o=order(kc$cluster)
data.frame(newpolicy$county[o],kc$cluster[o])
kc

## list of cluster assignments
o=order(kc$cluster)
data.frame(newpolicy$county[o],kc$cluster[o])

plot(newpolicy$policies, newpolicy$total_pay, type="n", xlim=c(3,19), xlab="policies", ylab="total_pay")
text(x=newpolicy$policies, y=newpolicy$total_pay, labels=newpolicy$county,col=kc$cluster+1)

#descriptive stat
describe.by(newpolicy, group=NULL)

#load the dataset
str(tmp_pol)
# distribution of class variable
y <- tmp_pol$Policies_gfx_Class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#("policies","total_pay")], centers=3, nstart=10)


#train.Policy$Policies_gfx_Class <- tmp1[complete.cases(tmp),]
str(train.Policy)
kc<-kmeans(train.Policy[, 3:4], 3, nstart = 10)
kc
table(train.Policy$Policies_gfx_Class, kc$cluster)


#Measureing the central
summary(tmp_pol)
tmp_pol$Payment_gfx_Class <- tmp_pol[complete.cases(tmp1),]
tmp_pol$Policies_gfx_Class <- tmp_pol[complete.cases(tmp1),]

#median
median(tmp_pol$policies, na.rm = TRUE)
median(tmp_pol$total_pay, na.rm = TRUE)
summary(tmp_pol$policies)
summary(tmp_pol$total_pay)
summary(tmp_pol$total_loss)

hist(tmp_pol$policies,
     col="gray",
     border = "black",
     prob=TRUE,
          xlim = c(1, 136),
     ylim = c(0, 1500),
     xlab = "Policy",
     main = "Policy #1")
lines(density(tmp_pol$policies), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

summary(tmp_pol$Policies_gfx_Class)
boxplot(tmp_pol$total_loss ~ tmp_pol$Policies_gfx_Class, xlab='Policies', ylab='total_loss',
main = "Boxplots of loss by ploicy")

boxplot(split(tmp_pol$total_pay, tmp_pol$Policies_gfx_Class),main='total loss by policies')
with(tmp_pol,plot(total_pay,total_loss))

par("mar")
par(mar=c(1,1,1,1))
str(tmp_pol)


#Run Set_Seed(1234)
ind<-sample(2, nrow(tmp_pol), replace=TRUE, prob=c(0.7, 03))
train.Policy <- tmp1[ind==1, ]
test.policy <- tmp1[ind==2, ]
str(train.Policy)
str(test.policy)


library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("Payment_gfx_Class", "Policies_gfx_Class", "state", "county")
continuous_vars <- c("policies", "insurance","premium", "total_loss", "closed_loss", "open_loss","cwop_loss","total_pay")

#compute IV for categories







#Run Set_Seed(1234)
ind<-sample(2, nrow(tmp_pol), replace=TRUE, prob=c(0.7, 03))
train.Policy <- tmp1[ind==1, ]
test.policy <- tmp1[ind==2, ]
str(train.Policy)
str(test.policy)


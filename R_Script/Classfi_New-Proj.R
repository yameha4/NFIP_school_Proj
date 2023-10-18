# required libraries
library(stringr)
library(ggplot2)
library(plyr)
library(ROCR)
library(ISLR)
library(gridExtra)
library(cluster)
library(randomForest)
install.packages("randomForest")
library(rpart)
library(tidyverse)
library(ISLR)
library(caret)
library(rpart.plot)
install.packages("rpart.plot")
library(e1071)
install.packages("e1071")
library(doMC)
install.packages("doMC")

# required functions
source("analyze_policies.R") # function to preprocess policies data
source("analyze_claims.R") # function to preprocess claims data

# policies data
policies <- Fn_Analyze_Policies()
# claims data
claims <- Fn_Analyze_Claims()

# combine county data on policies and claims
all_data <- merge(policies, claims, by = c("state", "county"), all = TRUE)
# convert state and county names to be consistent with ggplot2
all_data$state <- tolower(all_data$state)
all_data$county <- tolower(all_data$county)
# remove " county" and " parish" from county names
all_data$county <- gsub(" county", "", all_data$county)
all_data$county <- gsub(" parish", "", all_data$county)

# geo referencing info on counties and states
geo_county <- map_data("county")
names(geo_county) <- c("long", "lat", "group", "order", "state", "county")
geo_state <- map_data("state")


# data for graphics
gfx_data <- merge(geo_county, all_data, by = c("state", "county"))
gfx_data <- gfx_data[order(gfx_data$order), ]
# discretise variables of interest
gfx_data$policies_gfx <- cut(gfx_data$policies,
                             breaks = c(1, 10000, 400000),
                             labels = c("1 - 10k", "10k - 400k"))
gfx_data$payments_gfx <- cut(gfx_data$total_pay/10^6,
                             breaks = c(0, 0.01, 7300),
                             labels = c("0 - 10k", "10k - 7.3B"))

------------------------------------
#Export file and mnipulated in excel
gfx_data
write.table(gfx_data, "gfx_data.csv")

 -------------------------------------
 #Import the new excel file
setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(readxl)
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
tmp1$Payment_gfx_Class <- tmp1[complete.cases(tmp),]
tmp1$Policies_gfx_Class <- tmp1[complete.cases(tmp),]
NROW(tmp1)
summary(tmp1)
str(tmp1)

#tmp1$Payment_gfx_Class <- tmp1[!is.na(tmp1$Payment_gfx_Class), -211]
#tmp1$Policies_gfx_Class  <- tmp1[!is.na(tmp1$Policies_gfx_Class), -5]
#How many Polices and Pyaments are high and low
#Remove column
tmp_pol<- tmp1
#tmp_pol$Payment_gfx_Class<- NULL
tmp_pol$long<- NULL
tmp_pol$lat<- NULL
tmp_pol$group<- NULL
tmp_pol$order<- NULL

tmp_pol$county<- NULL
tmp_pol$state<- NULL
#tmp_pol$policies_gfx<- NULL
tmp_pol$Payment_gfx<- NULL
tmp_pol$Payment_gfx_Class<-NULL
tmp_pol$Policies_gfx_Class<-NULL

str(tmp_pol)
View(tmp_pol)
summary(tmp_pol)

#Run Set_Seed(1234)
ind<-sample(2, nrow(tmp_pol), replace=TRUE, prob=c(0.7, 03))
train.Policy <- tmp_pol[ind==1, ]
test.policy <- tmp_pol[ind==2, ]
str(train.Policy)
str(test.policy)

#Model devlopment
logit3 <- glm(Payment_gfx_Class ~ premium + total_loss, family='binomial', data=train.Policy)
summary(logit3)

table(train.Policy$Payment_gfx_Class)
d_tree <- rpart(Payment_gfx_Class ~ .-total_pay+total_loss+closed_loss, train.Policy)
rpart.plot(d_tree, main="Full Data Set Decision Tree"
           , fallen.leaves=FALSE, extra=104, box.palette="GnBu")

#### Predict test data set ####
summary(d_tree)
pred = predict(d_tree, type = "Class")
table(pred)

mode=rpart()
predictions <- predict(d_tree, newdata = test.policy[,c(8:10)])
confusionMatrix(d_tree, test.policy$Payment_gfx_Class)

#SVM
library(e1071)

train.Policy$state<-NULL
str(train.Policy)

x <- subset(train.Policy, select=-Payment_gfx_Class) #excluding Species
y <- Payment_gfx_Class

modelsvm <- svm(Payment_gfx_Class ~ ., data = train.Policy)
summary(modelsvm)

##Run Prediction and you can measuring the execution time in R
pred <- predict(modelsvm,x)
system.time(pred <- predict(modelsvm,x))

table(pred,Payment_gfx_Class)

# alternatively the traditional interface:
x <- subset(train.Policy, select = -Payment_gfx_Class)
y <- Payment_gfx_Class
model <- svm(x, y)

c(class(pred),class(train.Policy$Payment_gfx_Class))
table(pred,train.Policy$Payment_gfx_Class)


#svm test
# test with train data
pred <- predict(modelsvm, x)
# (same as:)
pred <- fitted(modelsvm)

# Check accuracy:
table(pred, train.Policy$Payment_gfx_Class)




dat <- data.frame(x=train.Policy$total_pay, y=as.factor(train.Policy$Payment_gfx_Class))
svm.fit <- svm(y ~., data=dat, kernel='linear', cost=10, scale=FALSE)
# Plot the SVC obtained
plot(svm.fit, dat)





p.roll.belt <- ggplot(train.Policy, aes(Payment_gfx_Class, roll_belt))
p.roll.belt <- p.roll.belt + geom_boxplot() + ggtitle("Roll Belt")
gridExtra::grid.arrange(p.roll.belt, p.pitch.belt, p.yaw.belt, p.accel.belt, ncol = 2, nrow = 2)

train.rf<-randomForest(Payment_gfx_Class ~ .-total_loss, data = train.Policy)



#random Forest evaluation
predictions.rf <- predict(logit3, newdata = testing[,c(8:60)])
confusionMatrix(predictions.rf, testing$classe)


#plot for random forest
p = ggplot(train.Policy,aes(x=Payment_gfx_Class,
                    y=state,
                    color=Payment_gfx_Class))

p + geom_jitter(alpha=0.3) +
  scale_color_manual(breaks = c('High','Low'),
                     values=c('darkgreen','red'))


plotROC(train.Policy$Payment_gfx_Class, predicted)

vif(logit3)
misClassError(test.Policy$Payment_gfx_Class, predicted, treshold = optCutOff)


misClassError(test.policy$Payment_gfx_Class, predicted, threshold = optCutOff)
plotROC(testData$Payment_gfx_Class, predicted)

confusionMatrix(testData$Payment_gfx_Class, predicted, threshold = optCutOff)
concordance(test.policy$Payment_gfx_Class, predicted)


#confustion Metrix on test dataset
prob <- predict(logit3, test.policy, type = 'response')
pred <- rep('>=10000K', length(prob))
pred[prob>=.001] <- '<10000k'
tb <- table(pred, test.policy$Payment_gfx_Class)
tb

# K-Means Clustering
train.Policy$Payment_gfx_Class<- NULL
table(train.Policy$Policies_gfx_Class)
set.seed(10)
#train.Policy$Policies_gfx_Class <- tmp1[complete.cases(tmp),]
str(train.Policy)
kc<-kmeans(train.Policy[, 3:4], 3, nstart = 10)
kc
table(train.Policy$Policies_gfx_Class, kc$cluster)


logit <- glm(Payment_gfx_Class ~ , data=train.Policy, family='binomial')
summary(logit)
#this means is that premium is associated with an increase or decrease of Policies class \(β1\) is -1.0

#A logistic regression using Policies_gfx_Class as the response variable, and premium + total_loss variables as predictors is fitted 97.5%.
confint(logit3)





# Create a new dummy variable for total pay
train.Policy$Payment_gfx_Class <- 0
train.Policy$Payment_gfx_Class[train.Policy$Payment_gfx_Class=="High"] <- 1

logit2 <- glm(Policies_gfx_Class ~ Payment_gfx_Class, data=train.Policy, family='binomial')
summary(logit2)





# create a data frame to store information regarding deviance residuals
index <- 1:dim(train.Policy)[1]
dev_resid <- residuals(logit3)
Policies_gfx_Class <- train.Policy$Policies_gfx_Class
dff <- data.frame(index, dev_resid, Policies_gfx_Class)




ggplot(dff, aes(x = index, y = dev_resid, color = Policies_gfx_Class)) +
  geom_point() +
  geom_hline(yintercept = 3, linetype = 'dashed', color = 'blue') +
  geom_hline(yintercept = -3, linetype = 'dashed', color = 'blue')

ggtitle('Plot of Deviance Residuals')

#confustion Metrix
prob <- predict(logit3, test.policy, type = 'response')
pred <- rep('<=10000K', length(prob))
pred[prob>=.001] <- '>10000k'
# confusion matrix
tb <- table(pred, test.policy$Policies_gfx_Class)
tb

# 64342 The prediction result has an accuracy of 97.70%, and a misclassification rate of 2.18%.

#6512



sapply(train.Policy, table)
summary(train.Policy)
grid.arrange(y,z,nrow=1)
test.policy1 <- table(test.policy)
(train_Pldf[[2]]/train_Pldf[[1]])*100

train_Pldf<-as.data.frame(train.Policy$Policies_gfx_Class)
str(train_Pldf)
(train_Pldf[[2]]/train_Pldf[[1]])*100


x <- qplot(x=premium, y=total_loss, color=Policies_gfx_Class, shape=Policies_gfx_Class, geom='point')+scale_shape(solid=FALSE)
y <- qplot(x=Policies_gfx_Class, y=premium, fill=Policies_gfx_Class, geom='boxplot')+guides(fill=FALSE)
z <- qplot(x=Policies_gfx_Class, y=total_loss, fill=Policies_gfx_Class, geom='boxplot')+guides(fill=FALSE)

#plot
x
summary(x)
grid.arrange(y,z,nrow=1)













New_all_data<-all_data
colnames(New_all_data)<-c( 'state', 'county', 'policies',
                           'insurance', 'premium', 'total_loss',
                           'closed_loss', 'open_loss', 'cwop_loss', 'total_pay')

ggplot(New_all_data) + aes(x=as.numeric(premium), group=, fill=income) +
  geom_histogram(binwidth=1, color='black')







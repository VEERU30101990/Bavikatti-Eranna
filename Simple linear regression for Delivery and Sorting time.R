###############Load dataset of Delivery and sorting Time###########

dt.st <- read.csv("D:\\ASSIGNMENTS OF EXCELr\\Simple Linear Regression _04\\delivery_time.csv")
View(dt.st) #dt = delivery time, st = sorting time
summary(dt.st)
range(dt.st$dt)
range(dt.st$st)
sd(dt.st$dt)
var(dt.st$dt)
sd(dt.st$st)
var(dt.st$st)

# install.packages("lattice")
library("lattice")
?lattice
?dotplot

############## Graphical visualizations #############################

dotplot(dt.st$dt, main="Dot Plot of  delivery time")
dotplot(dt.st$st, main="Dot Plot of  sorting time ")
?dotplot

boxplot(dt.st$dt,col="orange")
boxplot(dt.st$st,col="blue", horizontal = T)
?boxplot

hist(dt.st$dt)
hist(dt.st$st)
?hist

qqnorm(dt.st$dt)
qqline(dt.st$dt)
qqnorm(dt.st$st)
qqline(dt.st$st)
?qqnorm

hist(dt.st$dt, prob=T)
hist(dt.st$st, prob=T)

#################Scatter plot############################

plot(dt.st$dt,dt.st$st,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="red", xlab="Sorting Time", 
     ylab="Delivery Time", pch=25)  # plot(x,y)

?plot
########  simple command #######
#scatter plot
attach(dt.st)
View(dt.st)
plot(dt,st)
cor(dt,st)

##################### Simple Linear Regression################
##### using different model of transform the variables #####

reg <- lm(dt~st, data=dt.st) # Y ~ X
summary(reg) ##### R-squared = 0.6706
confint(reg,level=0.95)
?confint
?predict
pred <- predict(reg,interval="predict")
pred
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,dt)

##### transform the variables to check whether the predicted values are better ######

########################### MODEL 1 ###################

reg_sqrt <- lm(dt~sqrt(st), data=dt.st)
summary(reg_sqrt)###### R-squared = 0.6852
confint(reg_sqrt,level=0.95)
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)
View(pred1)
cor(pred1$fit, dt.st$dt)

########################### MODEL 2 ###########################

reg1<-lm(log(dt)~st + I(st*st), data=dt.st)
summary(reg1) ######## R-squared = 0.7552
confint(reg1,level=0.95)
pred3<-predict(reg1,interval="predict")
pred3
pred3 <- as.data.frame(pred3)
pred3 <- exp(pred3$fit)
pred3
cor(pred3,dt)

########################### MODEL 4 ###################

reg_sqrt1<-lm(sqrt(dt)~st, data=dt.st)
summary(reg_sqrt1)######## R-squared = 0.6927
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")
pred4<-predict(reg_sqrt1,interval="predict")
pred4
pred4 <-as.data.frame(pred4)
pred4
cor(pred4$fit,dt)

########################### MODEL 5 ###################

reg_log1<-lm(log(dt)~st, data=dt.st)
summary(reg_log1)############  R-squared = 0.7001
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")
pred5 <-predict(reg_log1, interval="predict")
pred5
pred5 <-as.data.frame(pred5)
pred5 <- exp(pred5$fit)
pred5
cor(pred5,dt)

#####  the models which is the Highest R- squared value is better model #########
    ######  MODEL is the better model having R- squared is 0.75 ######

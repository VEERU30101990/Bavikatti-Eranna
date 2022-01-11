################ Load the dataset of Salary Data ############

dataset <- read.csv("D:\\ASSIGNMENTS OF EXCELr\\Simple Linear Regression _04\\Salary_Data.csv")
View(dataset)
summary(dataset)
range(dataset$YearsExperience)
range(dataset$Salary)
sd(dataset$YearsExperience)
var(dataset$YearsExperience)
sd(dataset$Salary)
var(dataset$Salary)

# install.packages("lattice")
library("lattice")
?lattice
?dotplot
############ Graphical Visualizations ##################

dotplot(dataset$YearsExperience, main="Dot Plot of YearsExperience")
dotplot(dataset$Salary, main="Dot Plot of Salary")
?dotplot

boxplot(dataset$YearsExperience,col="dodgerblue4")
boxplot(dataset$Salary,col="red", horizontal = T)
?boxplot

hist(dataset$YearsExperience)
hist(dataset$Salary)
?hist

qqnorm(dataset$YearsExperience)
qqline(dataset$YearsExperience)
qqnorm(dataset$Salary)
qqline(dataset$Salary)
?qqnorm


hist(dataset$YearsExperience, prob=T)   
hist(dataset$Salary, prob=T)
####################### Scatter plot #########################
plot(dataset$YearsExperience, dataset$Salary,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Salary", 
     ylab="YearsExperience", pch=21)  # plot(x,y)

?plot
############ alternate simple command ##################
#############scatter plot##################
attach(dataset)
View(dataset)
plot(Salary,YearsExperience)
cor(Salary, YearsExperience)

##################### Simple Linear Regression################
##### using different model of transform the variables #####

reg <- lm(YearsExperience~Salary, data=dataset) # Y ~ X
summary(reg)######## R-squared = 0.957
confint(reg,level=0.95)
?confint
?predict
pred <- predict(reg,interval="predict")
pred
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,YearsExperience)

##### transform the variables to check whether the predicted values are better ######

########################### MODEL 1 ##########################

reg_sqrt <- lm(YearsExperience~sqrt(Salary), data=dataset)
summary(reg_sqrt)####### R-squared = 0.9498
confint(reg_sqrt,level=0.95)
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)
pred1
cor(pred1$fit, dataset$YearsExperience)

########################### MODEL 2 ##########################

reg_log<-lm(YearsExperience~log(Salary), data=dataset)
summary(reg_log) ######### R-squared = 0.932
confint(reg_log,level=0.95)
pred2 <- predict(reg_log,interval="predict")
pred2
pred2 <- as.data.frame(pred2)
View(pred2)
cor(pred2$fit, YearsExperience) 

########################### MODEL 3 ##########################

reg1<-lm(log(YearsExperience)~Salary + I(Salary*Salary), data=dataset)
summary(reg1) ########### R-squared = 0.9074
confint(reg1,level=0.95)
pred3<-predict(reg1,interval="predict")
pred3
pred3 <- as.data.frame(pred3)
pred3 <- exp(pred3$fit)
pred3
cor(pred3,YearsExperience)

#####  the models which is the Highest R- squared value is better model #########
######  MODEL is the better model having R- squared is 0.957 #########


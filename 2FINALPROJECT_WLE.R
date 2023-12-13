setwd("~/Documents/UHD MSDA Program/STAT5310.FALL23/FINALPROJECT")
library(readxl)
library(ggplot2)
female.salary <- read_excel("SalaryData.xlsx", sheet = "Female")

female.salary$EducationLevel <- factor(female.salary$EducationLevel)
female.salary$JobTitle <- factor(female.salary$JobTitle)

male.salary <- read_excel("SalaryData.xlsx", sheet = "Male")
male.salary$EducationLevel <- factor(male.salary$EducationLevel)
male.salary$JobTitle <- factor(male.salary$JobTitle)


#Normality
hist(female.salary$Salary, col="pink",xlab = "Salary", main = "Female Salary")
hist(male.salary$Salary, col="lightblue", xlab = "Salary", main = "Male Salary")
#Linearity

plot(female.salary$Salary ~ female.salary$Age, data = female.salary , col="pink", xlab = "Age", ylab ="Salary")
plot(female.salary$Salary ~ female.salary$EducationLevel, data = female.salary, col = "pink", xlab = "Education Level", ylab ="Salary")
plot(female.salary$Salary ~ female.salary$Experience, data = female.salary, col="pink", xlab = "Year of Experience", ylab ="Salary")
plot(female.salary$Salary ~ female.salary$JobTitle, data = female.salary, col="pink",xlab = "Job Title", ylab ="Salary")

plot(male.salary$Salary ~ male.salary$Age, data = male.salary , col="lightblue", xlab = "Age", ylab ="Salary")
plot(male.salary$Salary ~ male.salary$EducationLevel, data = male.salary, col = "lightblue", xlab = "Education Level", ylab ="Salary")
plot(male.salary$Salary ~ male.salary$Experience, data = male.salary, col="lightblue", xlab = "Year of Experience", ylab ="Salary")
plot(male.salary$Salary ~ male.salary$JobTitle, data = male.salary, col="lightblue", xlab = "Job Title", ylab ="Salary")

#Running model
female <- lm(Salary ~ Age+EducationLevelCode+JobTitle+Experience, data = female.salary)
summary(female)
plot(female$fitted.values, female$Salary, col= "pink", xlab = "Fitted Values", ylab = "Salary")
male <- lm(Salary ~ Age+EducationLevelCode+JobTitle+Experience, data = male.salary)
summary(male)
plot(male$fitted.values, male$Salary, col= "lightblue", xlab = "Fitted Values", ylab = "Salary")
#Running VIF
library(car)
car::vif(female)
car::vif(male)
#Plotting the correlation between variables
pairs(female.salary[,c("Salary","Age","EducationLevel","Experience","JobTitle")], col = "pink")
pairs(male.salary[,c("Salary","Age","EducationLevel","Experience","JobTitle")], col = "lightblue")
cor(female.salary[,c("Age","Experience")])
cor(male.salary[,c("Age","EducationLevelCode","Experience")])

#Using forward method to find the model:
#AIC
female.fw <- lm(Salary ~ 1, data = female.salary)
femalefwAIC <- step(female.fw, scope = list(lower=~1, upper=~Age+EducationLevelCode+Experience+JobTitle), direction="forward",data=female.salary)
male.fw <- lm(Salary ~ 1, data = male.salary)
malefwAIC <- step(male.fw, scope = list(lower=~1, upper=~Age+EducationLevelCode+Experience+JobTitle), direction="forward",data=male.salary)
#BIC
femalefwBIC <- step(female.fw, scope = list(lower=~1, upper=~Age+EducationLevelCode+Experience+JobTitle), direction="forward",data=female.salary, k = log(n))
malefwBIC <- step(male.fw, scope = list(lower=~1, upper=~Age+EducationLevelCode+Experience+JobTitle), direction="forward",data=male.salary, k = log(n))
#Using backward method to find the model:
#AIC
femalebwAIC<-step(female,direction = "backward", data= female.salary)
malebwAIC<-step(male,direction = "backward", data= male.salary)
#BIC
femalebwBIC<-step(female,direction = "backward", data= female.salary, k=log(n))
malebwBIC<-step(male,direction = "backward", data= male.salary, k=log(n))
#For female, using BIC to choose the best model, both backward and forward method decided Salary~Age+EducationLevelCode as the optimal model for female
#For male, backward method's AIC recommended Salary ~ Age+EdecationLevelCode+JobTitle+Experience, however, both BIC and AIC from forward method and BIC on backward method agreed with Salary ~ JobTitle+Experience as the optimal model

#Running linear models
femalemodel <- lm(Salary ~ Age + EducationLevelCode, data = female.salary)
summary(femalemodel)
mmps(femalemodel,layout=c(1,3), col = "pink")
plot(femalemodel$fitted.values, female.salary$Salary, xlab = "Fitted Value", col = "pink")
abline(lsfit(femalemodel$fitted.values,female.salary$Salary), col = "red")
malemodel <- lm(Salary ~ JobTitle + Experience, data = male.salary)
mmps(malemodel, col="lightblue")
plot(malemodel$fitted.values, male.salary$Salary, xlab = "Fitted Value", col = "lightblue")
abline(lsfit(malemodel$fitted.values,male.salary$Salary), col = "red")
summary(malemodel)

#Rsquared and AdjustedRsquared

summary(femalemodel)$r.squared
summary(femalemodel)$adj.r.squared
summary(malemodel)$r.squared
summary(malemodel)$adj.r.squared

table <- data.frame(Description = c("Female", "Male"),
                    Orig.model.Rsquared = c(summary(female)$r.squared,summary(male)$r.squared),
                    Orig.model.Adj.Rsquared = c(summary(female)$adj.r.squared,summary(male)$adj.r.squared),
                    New.model.Rsquared = c(summary(femalemodel)$r.squared,summary(malemodel)$r.squared),
                    New.model.Adj.Rsquared = c(summary(femalemodel)$adj.r.squared,summary(malemodel)$adj.r.squared)
)
table
malesummary<-summary(malemodel)
malesummary$coefficients[, "Pr(>|t|)"]
# Example: Assuming 'my_model' is your linear regression model
# Fit your linear regression model (replace this with your actual model)
malemodel <- lm(Salary ~ JobTitle + Experience, data = male.salary)

# Extract the p-values for interactions
interaction_terms <- attr(summary(malemodel)$coefficients, "dimnames")[[1]]
p_values_interactions <- summary(malemodel)$coefficients[, "Pr(>|t|)"]
# Create a matrix with interaction terms and their p-values
interaction_p_values <- matrix(p_values_interactions, ncol = 1)
rownames(interaction_p_values) <- interaction_terms
# Convert the matrix to a table using xtable package
library(xtable)
# Create a table object
table_obj <- xtable(interaction_p_values, caption = "Regressor Interactions and P-Values")
table_obj
write.table(table_obj, file = "table.csv", sep = "\t", row.names=TRUE)

par(mfrow = c(2,2)) 
plot(femalemodel, col="pink")
plot(malemodel, col="lightblue")

par(mfrow = c(1,1))
library(boot)
set.seed(1)
train.female <- sample(c(TRUE,FALSE),nrow(female.salary), rep = TRUE)
test.female <- (!train.female)
female.training.data <- female.salary[train.female,]
female.test.data <- female.salary[test.female,]
femaletrainmodel <- lm(Salary~Age+EducationLevelCode, data = female.training.data)
female.pred.salary <- predict(femaletrainmodel, female.test.data, type = "response")
mean((female.pred.salary-female.test.data$Salary)^2)

library(caTools)
male.salary$JobTitle <- as.factor(male.salary$JobTitle)
set.seed(123)
splitindex <- sample(1:nrow(male.salary), size = 0.5*nrow(male.salary))

train.male <- sample(c(TRUE,FALSE),nrow(male.salary), rep = TRUE)
test.male <- (!train.male)
male.training.data <- male.salary[splitindex,]
male.test.data <- male.salary[-splitindex,]
maletrainmodel <- lm(Salary~JobTitle + Experience, data = male.training.data)

male.test.data$JobTitle <- as.factor(male.test.data$JobTitle)
male.pred.salary <- predict(maletrainmodel, male.test.data, type = "response")
mean((male.pred.salary-male.test.data$Salary)^2)



boot.fn = function(data, index){
  mu <- mean(data[index])
  return(mu)}
SE.boot.male <- boot(male.salary$Salary, boot.fn, 1000)
muhat.male <- SE.boot.male$t0
se.male<- sd(SE.boot.male$t)
CImale <- c(muhat.male-2*se.male, muhat.male+2*se.male)
CImale
t.test(male.salary$Salary)


SE.boot.female <- boot(female.salary$Salary, boot.fn, 1000)
muhat.female <- SE.boot.female$t0
se.female<- sd(SE.boot.female$t)
CIfemale <- c(muhat.female-2*se.female, muhat.female+2*se.female)
CIfemale
t.test(female.salary$Salary)






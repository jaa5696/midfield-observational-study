library(data.table)
library(midfielddata)
library(Metrics)
library(caret)
library(rcompanion)

set.seed(461)

train<-fread("master_glm.csv")
train$sex<-as.factor(train$sex)
train$race<-as.factor(train$race)
train<-train[race!="Unknown" & sex!="Unknown"]
Gender <- factor(train$sex, levels=c('Male', 'Female'))
Race<-factor(train$race, levels=c("White", "Asian", "Black", "Hispanic", "International", "Native American", "Other"))

#Assumptions:
#Outcome structure: Obvious
#Independent observations: Needs some work
#No Multicollinearity
dummies <- caret::dummyVars(dropout~race+sex, data=train)
fake_train<-predict(dummies, newdata=train)
fake_train<-data.table(fake_train)

cor(fake_train[,1:10])#None above .7 (well other than male to female but, well, duh!)
#Linearity of independent vars and log odds
#large sample size (obvious)


glm_model<-glm(dropout~Gender+Race,family=binomial)
summary(glm_model)

glm_modeli<-glm(dropout~Gender+Race+Gender:Race,family=binomial,data=train)
summary(glm_modeli)

compareGLM(glm_modeli, glm_model)

#This gives the prediction for all pairings
test<-data.table(Race = c("White", "Asian", "Black", "Hispanic", "International", "Native American", "Other", "White", "Asian", "Black", "Hispanic", "International", "Native American", "Other"), Gender=c("Male","Male","Male","Male","Male","Male","Male","Female","Female","Female","Female","Female","Female","Female"))
test$pred<-predict(glm_model,newdata = test,type="response")

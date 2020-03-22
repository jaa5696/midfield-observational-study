library(data.table)
library(midfielddata)
library(Metrics)
library(caret)

set.seed(461)

#Take all students' race and gender then append some currently totally fake drop out data
fake_train<-midfieldstudents[,c("sex", "race")]
fake_train<-as.data.table(fake_train)
fake_train<-fake_train[,drop_out:=sample(c(0,1), nrow(fake_train), replace=TRUE)]
fake_train<-fake_train[race!="Unknown" & sex!="Unknown"]
fake_data<-fake_train[,drop_out]


#dummy variables: this turns the race and gender into binary variables
dummies <- caret::dummyVars(drop_out~., data=fake_train)
fake_train<-predict(dummies, newdata=fake_train)
fake_train<-data.table(fake_train)
fake_train$drop_out<-fake_data

#Remove the male gender and the other race columns since they are already decided by the remaining races/genders
fake_train<-fake_train[,-c("sexMale", "raceOther")]

#This line fits the logistic regression; binomial is a natural family to use since we are predicting 0's and 1's
glm_model<-glm(drop_out~.,family=binomial,data=fake_train)

#look at the model
summary(glm_model)
coef(glm_model)

#Note that since the drop out column was generated completely randomly, it is unsurprising that the variables don't appear to be significant predictors
#Other considerations:
  #We could limit the number of races (basically group more into the other category)
  #I'm going to have to think on the interpretation of the excluded race; may be better to just include it (but in theory we shouldn't have to because it's like the gender case)


cardio_train <- read.csv('cardio_train.csv',sep=',')
##getting age in years 
cardio_train$age_in_years<-cardio_train$age/365
cardio_train$age_in_years<- round(cardio_train$age_in_years,digits = 0)

#getting the summary statistics for all the columns 
summary(cardio_train)


library(ggplot2)
library(dplyr)
library(corrplot)

##getting the correlation
cor_matrix<-cor(cardio_train)
head(cor_matrix)


corrplot(cor_matrix, method="circle")


#plot for getting the count of gender
gender_table<-table(cardio_train$gender)
gender_table<-as.data.frame(gender_table)
ifelse(gender_table$Var1==1,'Male','Female')
ggplot(gender_table, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill = gender_table$Var1)+ggtitle("Count of Male and Female Patients") +
  xlab("Gender") + ylab("Count")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))



#finding out count of smokers and non-smokers 
smoke<- table(cardio_train$smoke)
smoke<- as.data.frame(smoke)
ggplot(smoke, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill = "steel blue")+ggtitle("Count of Smokers and Non-Smokers") +
  xlab("Smoke") + ylab("Count")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))

#finding out count of patients who drink alcohol 
drink<- table(cardio_train$alco)
drink<- as.data.frame(drink)
ggplot(drink, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill = "steel blue")+ggtitle("Count of alcoholics and Non-alcoholics") +
  xlab("Alcohol") + ylab("Count")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))

#plotting cholestrol levels
chol<- table(cardio_train$cholesterol)
chol<- as.data.frame(chol)
ggplot(chol, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill=chol$Var1)+ggtitle("Levels of cholestrol in Patients") +
  xlab("Cholestrol") + ylab("Count")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))


#catogerising the ages into groups

cardio_train$agegroup[cardio_train$age_in_years >= 30 & cardio_train$age_in_years<=40] = "Thirties"
cardio_train$agegroup[cardio_train$age_in_years >= 41 & cardio_train$age_in_years<=50] = "Forties"
cardio_train$agegroup[cardio_train$age_in_years >= 51 & cardio_train$age_in_years<=60] = "Fifties"
cardio_train$agegroup[cardio_train$age_in_years >= 61 & cardio_train$age_in_years<=65] = "Sixties"

agetab<-table(cardio_train$agegroup)
agetab<-as.data.frame(agetab)


#getting the frequencies of different age groups

ggplot(agetab, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill = 'Black')+ggtitle("Data frequency in different agegroups") +
  xlab("Agr Group") + ylab("Count")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))



#converting Cardio(presence of cardiovascular disease to yes and no)
cardio_train$cardio[cardio_train$cardio== 1]="Yes"
cardio_train$cardio[cardio_train$cardio== 0]="No"

#checking the data for frequency of presence of heart disease
disease<-table(cardio_train$cardio)
disease<-as.data.frame(disease)
ggplot(disease, aes(x = Var1, y = Freq))+ geom_bar(stat="identity", fill = "steel blue") +ggtitle("Presence of Cardio Vacular Disease") +
  xlab(" Cardio Vacular Disease")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))




#table with count of gender with and without heart disease
gender_cardio<-xtabs(~ cardio + gender, data = cardio_train)
gender_cardio<-as.data.frame(gender_cardio)

#grouped barplot representing males and females with and without heart disease 
ggplot(gender_cardio, aes(x = gender, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with wrt Gender ") +
  xlab(" Gender")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))



#table with count of age groups with and without heart disease
age_cardio<- xtabs(~ cardio + agegroup, data = cardio_train)
age_cardio<-as.data.frame(age_cardio)

#grouped barplot representing different age groups with and without heart disease 


ggplot(age_cardio, aes(x = agegroup, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to Age Groups ") +
  xlab(" Age Group")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))


#table with count of cholestrol with and without heart disease

chol_cardio<- xtabs(~ cardio + cholesterol, data = cardio_train)
chol_cardio<-as.data.frame(chol_cardio)

#grouped barplot representing different cholestrol levels with and without heart disease 

ggplot(chol_cardio, aes(x = cholesterol, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to Cholestrol ") +
  xlab(" Cholestrol Level")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))

#table with count of glucose with and without heart disease

gluc_cardio<- xtabs(~ cardio + gluc, data = cardio_train)
gluc_cardio<-as.data.frame(gluc_cardio)

#grouped barplot representing different glucose levels with and without heart disease 

ggplot(gluc_cardio, aes(x = gluc, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to glucose Level") +
  xlab(" Glucose Level")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))


#table with count of smoke with and without heart disease

smoke_cardio<- xtabs(~ cardio +smoke, data = cardio_train)
smoke_cardio<-as.data.frame(smoke_cardio)

#grouped barplot representing smokers and non-smokers with and without heart disease 

ggplot(smoke_cardio, aes(x = smoke, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to Smoking") +
  xlab(" Smoker/Non-smoker")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))


#table with count of aclo with and without heart disease

alco_cardio<- xtabs(~ cardio +alco, data = cardio_train)
alco_cardio<-as.data.frame(alco_cardio)

#grouped barplot representing Alcoholic/Non-Alcoholic with and without heart disease 

ggplot(alco_cardio, aes(x = alco, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to Alcohol Consumption") +
  xlab(" Alcoholic/Non-Alcoholic")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))

#table with count of active with and without heart disease

active_cardio<- xtabs(~ cardio +active, data = cardio_train)
active_cardio<-as.data.frame(active_cardio)

#grouped barplot representing active/non-active with and without heart disease 

ggplot(active_cardio, aes(x = active, y = Freq, fill = cardio)) +
  geom_col(position = "dodge")+ggtitle("Presences of Heart Disease with respect to Activity of Patient") +
  xlab(" Active/Non-Active")+ ylab("Number of Cases")+ theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))


##logistic regression to classify cardio-vascular diseases
cardio_train$cardio<- as.factor(cardio_train$cardio)


#Splitting data into test and train data and performing logistic regression
cardio_train$cardio<- ifelse(cardio_train$cardio=='Yes',1,0)
set.seed(75)

train_ind <- sample(nrow(cardio_train),0.75*nrow(cardio_train))
length(train_ind)
train1 <- cardio_train[train_ind,]
test1 <- cardio_train[-train_ind,]
logistic1<- glm(cardio ~., data = train1, family = "binomial")
pred1<- (predict(logistic1,test1,type='response'))
#Evaluation and understanding of model
library(caret)
library(lattice)
library(InformationValue)
library(Metrics)
varImp(logistic1)
opt<- optimalCutoff(test1$cardio,pred1)

summary(logistic1)
exp(logistic1$coefficients)

misClassError(test1$cardio,pred1,threshold=opt)
plotROC(test1$cardio,pred1)
sensitivity <- sensitivity(test1$cardio,pred1,opt)
specificity <- specificity(test1$cardio,pred1,opt)
confusionMatrix(test1$cardio,pred1)


#dataset to represent probability of having a cardio-vascular disease
train_data<-data.frame(probability_of_cardio = logistic1$fitted.values, cardio = train1$cardio)
#sorting data
train_data<- train_data[ order(train_data$probability_of_cardio, decreasing = FALSE),]
#ranking from low probability to high probability
train_data$rank <- 1:nrow(train_data)


#plotting the data to represent the model 
library(ggplot2)
library(cowplot)

ggplot(data = train_data, aes(x= rank, y= probability_of_cardio)) +
  geom_point(aes(color=cardio))+
  xlab("Index")+ ylab("Predicted probability of getting a Heart Disease")



#NB
cardio_train$age_in_years<-cardio_train$age/365
cardio_train$age_in_years<- round(cardio_train$age_in_years,digits = 0)
cardio_train<- cardio_train[,-c(1:2)]

cardio_train$cardio<-as.factor(cardio_train$cardio)


cardio_train$gender<-as.factor(cardio_train$gender)


cardio_train$cholesterol<-as.factor(cardio_train$cholesterol)


cardio_train$gluc<-as.factor(cardio_train$gluc)


cardio_train$smoke<-as.factor(cardio_train$smoke)


cardio_train$alco<-as.factor(cardio_train$alco)


cardio_train$active<-as.factor(cardio_train$active)

cardio_train<- subset(cardio_train, select= c(1,6:11))


train.index <- sample(c(1:dim(cardio_train)[1]), dim(cardio_train)[1]*0.75) 
train.df <- cardio_train[train.index,] 
valid.df <- cardio_train[-train.index,]

library(e1071)
cardio.nb <- naiveBayes(cardio ~ ., data = train.df) 
cardio.nb 

## predict probabilities
pred.prob <- predict(cardio.nb, newdata = valid.df, type = "raw")
## predict class membership 
pred.class <- predict(cardio.nb, newdata = valid.df)
df <- data.frame(actual = test1$cardio, predicted = pred.class, pred.prob)
head(df)

library(caret)
# training 
pred.class <- predict(cardio.nb, newdata = train.df) 
pred.class
train.df$cardio <- as.factor(train.df$cardio)
train.df$cardio <- as.numeric(train.df$cardio)
train.df$cardio<- ifelse(train.df$cardio==2,0,1)

caret::confusionMatrix(pred.class, train.df$cardio,positive="1")
# validation 
pred.class <- predict(cardio.nb, newdata = valid.df) 
caret::confusionMatrix(pred.class, valid.df$cardio,positive="1") 



#CLASSIFICATION TREE

library(rpart)
library(rpart.plot)
library(caret)

prp(ct_model, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(ct_model$frame$var == "<leaf>", 'gray', 'white'))

set.seed(75)

train_ind <- sample(nrow(cardio_train),0.75*nrow(cardio_train))
length(train_ind)
train1 <- cardio_train[train_ind,]
test1 <- cardio_train[-train_ind,]
ct_model <- rpart(cardio ~ ., data = train1, method = "class")

predict_train_ct<- predict(ct_model,test1,type = "class")
test1$cardio <- as.numeric(test1$cardio)
confusionMatrix(predict_train_ct, test1$cardio)


#Pruned Tree
cv.ct <- rpart(cardio ~ ., data = train.df, method = "class",
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

predict_train_ct_pruned<- predict(pruned.ct,train.df,type = "class")
train.df$cardio <- factor(train.df$cardio)
train.df$cardio <- as.numeric(train.df$cardio)
train.df$cardio<- ifelse(train.df$cardio==2,0,1)
caret::confusionMatrix(predict_train_ct_pruned, train.df$cardio,positive="1")

predict_valid_ct_purned<- predict(pruned.ct,valid.df,type = "class")
caret::confusionMatrix(predict_valid_ct_purned, valid.df$cardio,positive="1")




#Random Forest
library(randomForest)
## random forest
rf_model <- randomForest(cardio ~ ., data = train.df, ntree = 500,
                         mtry = 4, nodesize = 5, importance = TRUE)
## variable importance plot
varImpPlot(rf_model, type = 1)
## confusion matrix
rf.pred.train <- predict(rf_model,train.df)
caret::confusionMatrix(rf.pred.train, train.df$cardio, positive = "1")



rf.pred <- predict(rf_model, valid.df)
caret::confusionMatrix(rf.pred, valid.df$cardio, positive = "1")
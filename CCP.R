setwd('Customer-Churn-Prediction')
getwd()
dir()
data=read.csv('Customer Churn Data.csv',header=T,sep=',')
ls()
str(data)

#creating derived variables

data$total_calls <- data$total_night_calls + data$total_eve_calls + data$total_day_calls
data$total_charge <- data$total_night_charge + data$total_eve_charge + data$total_day_charge
data$total_minutes <- data$total_night_minutes + data$total_eve_minutes + data$total_day_minutes

data <- data[,-(9:17),drop=FALSE]
data <- data[,-(4:5),drop=FALSE]
data <- data[,-(1:2),drop=FALSE]

data <- data[,c(1,2,3,4,5,6,7,8,10,11,12,9)]
str(data)


#installed R caret package

library(caret)

train.index <- createDataPartition(data$churn, p = .8, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]
summary(factor(data$churn))
summary(factor(train$churn))
summary(factor(test$churn))

#installed e1071 package

library(e1071)

nb.model<-naiveBayes(train[,1:11], train[,12])
nb.model

#installed tree package

library(tree)

tree.model <- tree(train$churn~., data = train)
summary(tree.model)
plot(tree.model)
text(tree.model,pretty=0)

svm.model <- svm(train$churn~.,data=train)
summary(svm.model)


nb.pred = predict(nb.model,test[,-12])
table(pred=nb.pred,test[,12])
nb<-confusionMatrix(nb.pred,test[,12])
accuracy.nb <-nb$overall['Accuracy']
precision.nb <- nb$byClass['Pos Pred Value']    
recall.nb <- nb$byClass['Sensitivity']
accuracy.nb
precision.nb
recall.nb

tree.pred <- predict(tree.model, test, type="class")
table(pred=tree.pred,test$churn)
tree<-confusionMatrix(tree.pred,test$churn)
accuracy.tree <-tree$overall['Accuracy']
precision.tree <- tree$byClass['Pos Pred Value']    
recall.tree <- tree$byClass['Sensitivity']
accuracy.tree
precision.tree
recall.tree

svm.pred <- predict(svm.model, test[,-12])
table(pred = svm.pred,test[,12])
svm<-confusionMatrix(svm.pred,test[,12])
accuracy.svm <-svm$overall['Accuracy']
precision.svm <- svm$byClass['Pos Pred Value']    
recall.svm <- svm$byClass['Sensitivity']
accuracy.svm
precision.svm
recall.svm

#installed ROCR package

library(ROCR)

pr.nb <- prediction(as.numeric(nb.pred), as.numeric(test$churn))
prf.nb <- performance(pr.nb, "tpr", "fpr")
plot(prf.nb, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

pr.tree <- prediction(as.numeric(tree.pred), as.numeric(test$churn))
prf.tree <- performance(pr.tree, "tpr", "fpr")
plot(prf.tree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

pr.svm <- prediction(as.numeric(svm.pred), as.numeric(test$churn))
prf.svm <- performance(pr.svm, "tpr", "fpr")
plot(prf.svm, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

auc.nb<-as.numeric(performance(pr.nb, "auc")@y.values)
auc.tree<-as.numeric(performance(pr.tree, "auc")@y.values)
auc.svm<-as.numeric(performance(pr.svm, "auc")@y.values)
auc.nb
auc.tree
auc.svm

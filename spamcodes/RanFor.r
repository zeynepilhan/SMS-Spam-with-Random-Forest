library (randomForest)
library(rpart)
library(prediction)
library(pROC)
getwd()
setwd("C:/Users/zeynep/Desktop")
x<-read.csv("spam.csv", header=TRUE)


set.seed(17)
data_set_size <- floor(nrow(x)/5)
indexes <- sample(1:nrow(x), size = data_set_size)
training <- x[-indexes,]
validation1 <- x[indexes,]


mimic_class = randomForest(as.factor(spam)~ ., data=training, ntree=100, mtry=2, importance=TRUE, na.action = na.roughfix)  
mimic_class
plot (mimic_class)


pred = predict (mimic_class, validation1[ , -770]) 
tablo= table ( observed = validation1 [ , 3], predicted = pred )
library(caret)
cmrf = confusionMatrix(as.factor(validation1$spam), as.factor(pred)) 
print(cmrf)


library(ROCR)
 prediction_for_roc_curve4 <- predict(mimic_class,validation1[,-770],type="prob")
 
sink ("prediction_for_roc_curve4.xls", append=TRUE)
print(prediction_for_roc_curve4)
sink()
library(pdp)        
library(vip) 
importance(mimic_class)
varImpPlot(mimic_class)
vip(mimic_class)
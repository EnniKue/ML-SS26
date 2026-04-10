
#1. Linear Regression

ppb=read.csv("student_5_purchase_regression.csv")
View(ppb)

#Speify our x and y
names(ppb)

#y : Flatprice, x=c(WebsiteVisits, Age)
head(ppb)

table(ppb$Age)

#Split the dataset to training and test
#s= 70% random rows (trainig)
s=sample(nrow(ppb),0.7*nrow(ppb))
dim(ppb)

ppb.training=ppb[s,]   
View(ppb.training)

#Rest im Testdataset -s
ppb.test=ppb[-s, ]
View(ppb.test)


#Constructing models

names(ppb)
models=list(
  m1=lm(PurchaseProbability~WebsiteVisits, data=ppb.training),
  m2=lm(PurchaseProbability~Age, data=ppb.training),
  m3=lm(PurchaseProbability~WebsiteVisits+Age, data=ppb.training)
  
)

y.pred=predict(models[["m2"]], newdata = ppb.test)
comp.test=data.frame(ppb.test$PurchaseProbability, y.pred)
View(comp.test)

#SSE

y.error=comp.test$ppb.test.PurchaseProbability-comp.test$y.pred
comp.test=cbind(comp.test, y.error)
View(comp.test)
head(y.error)
sum(y.error)

#square the error ^2
#check the models performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=ppb.test)
  sum((ppb.test$PurchaseProbability-pred)^2)
}  )

test_sse
barplot(test_sse,
        horiz=T)
best_model_name=names(which.min(test_sse))
best_model_name


#2. Decission Tree Regration
library(rpart)
library(rpart.plot)


head(ppb.training)
head(ppb.test)

#Model Construction

models=list(
  m1=rpart(PurchaseProbability~WebsiteVisits, data=ppb.training, method="anova"),
  m2=rpart(PurchaseProbability~Age, data=ppb.training, method="anova"),
  m3=rpart(PurchaseProbability~WebsiteVisits+Age, data=ppb.training, method="anova")
  
)


#Check the performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=ppb.test)
  sum((ppb.test$PurchaseProbability-pred)^2)
}  )

best_model_name=names(which.min(test_sse))
best_model_name

rpart.plot(models[["m3"]])


#3. Random Forest Regression

#Data Split
head(ppb.training)
head(ppb.test)

library(randomForest)

# Model construction

models=list(
  m1=randomForest(PurchaseProbability~WebsiteVisits, data=ppb.training),
  m2=randomForest(PurchaseProbability~Age, data=ppb.training),
  m3=randomForest(PurchaseProbability~WebsiteVisits+Age, data=ppb.training)
  
)


# Test the performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=ppb.test)
  sum((ppb.test$PurchaseProbability-pred)^2)
}  )

best_model_name=names(which.min(test_sse))
best_model_name
best_model=models[["m3"]]
varImpPlot(best_model)

y.pred=predict(best_model, newdata = ppb.test)

plot(ppb.test$PurchaseProbability, y.pred, pch=9, col="darkgreen",
     main="Best Model",
     xlab="Actual PurchaseProbability",
     ylab="Predicted PurchaseProbability")


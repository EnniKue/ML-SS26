
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

y.error=comp.test$fdt.test.FlatPrice-comp.test$y.pred
comp.test=cbind(comp.test, y.error)
View(comp.test)
head(y.error)
sum(y.error)

#square the error ^2
#check the models performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=fdt.test)
  sum((fdt.test$FlatPrice-pred)^2)
}  )

test_sse
barplot(test_sse,
        horiz=T)
best_model_name=names(which.min(test_sse))
best_model_name


#2. Decission Tree Regration
library(rpart)
library(rpart.plot)


head(fdt.training)
head(fdt.test)

#Model Construction

models=list(
  m1=rpart(FlatPrice~Size_m2, data=fdt.training, method="anova"),
  m2=rpart(FlatPrice~Bedrooms, data=fdt.training, method="anova"),
  m3=rpart(FlatPrice~Location, data=fdt.training, method="anova"),
  m4=rpart(FlatPrice~Size_m2+Bedrooms, data=fdt.training, method="anova"),
  m5=rpart(FlatPrice~Size_m2+Location, data=fdt.training, method="anova"),
  m6=rpart(FlatPrice~Location+Bedrooms, data=fdt.training, method="anova"),
  m7=rpart(FlatPrice~Size_m2+Bedrooms+Location, data=fdt.training, method="anova")
)

#Check the performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=fdt.test)
  sum((fdt.test$FlatPrice-pred)^2)
}  )

best_model_name=names(which.min(test_sse))
best_model_name

rpart.plot(models[["m5"]])


#####
#3. Random Forest Regression

#Data Split
head(fdt.training)
head(fdt.test)

library(randomForest)

# Model construction

models=list(
  m1=randomForest(FlatPrice~Size_m2, data=fdt.training),
  m2=randomForest(FlatPrice~Bedrooms, data=fdt.training),
  m3=randomForest(FlatPrice~Location, data=fdt.training),
  m4=randomForest(FlatPrice~Size_m2+Bedrooms, data=fdt.training),
  m5=randomForest(FlatPrice~Size_m2+Location, data=fdt.training),
  m6=randomForest(FlatPrice~Location+Bedrooms, data=fdt.training),
  m7=randomForest(FlatPrice~Size_m2+Bedrooms+Location, data=fdt.training)
)

# Test the performance
test_sse=sapply(models, function(model){
  pred=predict(model, newdata=fdt.test)
  sum((fdt.test$FlatPrice-pred)^2)
}  )

best_model_name=names(which.min(test_sse))
best_model_name
best_model=models[["m7"]]
varImpPlot(best_model)

y.pred=predict(best_model, newdata = fdt.test)

plot(fdt.test$FlatPrice, y.pred, pch=9, col="darkgreen",
     main="Best Model",
     xlab="Actual Price",
     ylab="Predicted Price")

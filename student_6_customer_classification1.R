#1. Logistic Regression

gdt=read.csv("student_6_customer_classification1.csv")

head(gdt)

#X=c(Gender, StudyHrs, Major)    y=Grade

#Data preparation
#ändern Gold, Silver, Bronze in 1,2,3
table(gdt$CustomerCategory)
gdt$CustomerCategory=ifelse(gdt$CustomerCategory=="Gold",1,
                 ifelse(gdt$CustomerCategory=="Silver",2,3))

table(gdt$CustomerCategory)
gdt$CustomerCategory=factor(gdt$CustomerCategory)


head(gdt)
#View(gdt)

#Split th dataset  into test and train
s=sample(nrow(gdt), 0.7*nrow(gdt))
s

gdt.training=gdt[s,]
gdt.test=gdt[-s,]

# model construction
library(nnet)

names(gdt)

models=list(
  m1=multinom(CustomerCategory~VisitsPerMonth, data=gdt.training, trace=F),
  m2=multinom(CustomerCategory~Age, data=gdt.training, trace=F),
  m3=multinom(CustomerCategory~VisitsPerMonth+Age, data=gdt.training, trace=F)
  
)

# Test the performance

test_error=sapply(models, function(model){
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$CustomerCategory)
}  )

barplot(test_error, horiz=T)

best_model_name=names(which.min(test_error))
best_model_name

#KNN k nearest neighbour
#View(gdt)

head(gdt)


#Scale VisitsPerMonth
SC.vpm=(gdt$VisitsPerMonth-mean(gdt$VisitsPerMonth))/(sd(gdt$VisitsPerMonth))
plot(density(SC.vpm))
plot(density(gdt$VisitsPerMonth))
###
library(class)

#Data
head(gdt.training)
head(gdt.test)



#Model construction
names(gdt)
predictor_sets=list(
  c("VisitsPerMonth"),
  c("Age"),
  c("VisitsPerMonth","Age")
  
)

model_names=c("m1","m2","m3")

knn_test=sapply(1:3, function(i){
  #scaling
  vars=predictor_sets[[i]]
  train_x=scale(gdt.training[,vars, drop=F])
  test_x=scale(gdt.test[,vars, drop=F],
               center=attr(train_x, "scaled:center"),
               scale=attr(train_x, "scaled:scale"))
  #predict mit 9 Nachbarn
  pred=knn(train=train_x, test=test_x, cl=gdt.training$CustomerCategory, k=11)
  #confusion matrix
  mean(pred != gdt.test$CustomerCategory)
})

names(knn_test)=model_names
knn_test
barplot(knn_test, 
        horiz=T)

best_model_name=names(which.min(knn_test))
best_model_name

#3. Decision Tree

library(rpart)
library(rpart.plot)

#data
head(gdt.training)
head(gdt.test)


#Models Construction

models=list(
  m1=rpart(CustomerCategory~VisitsPerMonth, data=gdt.training, method="class"),
  m2=rpart(CustomerCategory~Age, data=gdt.training, method="class"),
  m3=rpart(CustomerCategory~VisitsPerMonth+Age, data=gdt.training, method="class")
  
)

#test the performance

test_error=sapply(models, function(model){
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$CustomerCategory)
}  )

test_error
barplot(test_error, horiz=T)

best_model_name=names(which.min(test_error))
best_model_name


gdt=read.csv("student_6_customer_classification1.csv")
table(gdt$CustomerCategory)
gdt$CustomerCategory=ifelse(gdt$CustomerCategory=="Gold",1,
                            ifelse(gdt$CustomerCategory=="Silver",2,3))

table(gdt$CustomerCategory)
gdt$CustomerCategory=factor(gdt$CustomerCategory)


head(gdt)
#View(gdt)

#Split th dataset  into test and train
s=sample(nrow(gdt), 0.7*nrow(gdt))
s

gdt.training=gdt[s,]
gdt.test=gdt[-s,]


#4. Random Forest
library(randomForest)



#model Construction

models=list(
  m1=randomForest(CustomerCategory~VisitsPerMonth, data=gdt.training),
  m2=randomForest(CustomerCategory~Age, data=gdt.training),
  m3=randomForest(CustomerCategory~VisitsPerMonth+Age, data=gdt.training)

)
# test the performance
test_error=sapply(models, function(model){
  
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$CustomerCategory)
}  )

test_error
barplot(test_error, horiz=T)

best_model_name=names(which.min(test_error))
best_model_name







setwd("C:/Users/Hausdrache/Desktop/HAM/SS26/Machine Leaarning/Aufgabe")


#Data preprocessing /Datenvorbereitung

install.packages("tidyverse")
install.packages("mice")

library("tidyverse")
library("mice")

#Create a sample dataset

df1=data.frame(
  Gender=c("Female", "f", "Male", "Child", "m", "Female", "f"),
  Age=c(21, NA, 60, 22, -30, 60, 60), 
  Height=c(190,185,"6ft",191,189, "6ft","6ft"),
  Mar.Status=c(NA, NA, NA, "Single", NA, NA, NA)
)

view(df1)
names(df1)
df1

#Gender
table(df1$Gender)

#wrong Value
df1$Gender=gsub("Child", NA, df1$Gender)

table(df1$Gender)
df1

#Inconsistencies

df1$Gender=gsub("Female", "f", df1$Gender)
df1$Gender=gsub("Male", "m", df1$Gender)
table(df1$Gender)

df1

#Invalid Values /Ungültig

class(df1$Height)
table(df1$Height)

df1$Height=gsub("6ft", "183", df1$Height)

class(df1$Height)
summary(df1$Height)

df1$Height=as.numeric(df1$Height)
class(df1$Height)

summary(df1$Height)


#Duplication

df1.dupl=duplicated(df1)
df1.dupl

#Remove the duplicated row(s)
df1.1=unique(df1)
df1.1

#missing Values
x=read.csv("datana.csv")

View(x)
names(x)
head(x)
str(x)

names(x)
sum(is.na(x$age))
sum(is.na(x$gender))

colSums(is.na(x))
md.pattern(x)

#Calculations under NA values
mean(x$age, na.rm=T)
sd(x$income, na.rm=T)

#Test if NA are ocurring randomly

dm.na=is.na(x$age)
table(dm.na)

View(dm.na)

#Test if the way ist missing on age ist related to gender
chisq.test(x$gender, dm.na)

#p-value = 0.7162 we do not reject th H0
#H0: gender and dm.na are indipendent (noch related)
#data is missing randomly (estimation possible)

#Imputation /impute it

x.fill=mice(x)
y=complete(x.fill)

colSums(is.na(y))


#Outliers

names(y)

#do we have outliers?
boxplot(y$income, horizontal = T)
boxplot.stats(y$income)

#remove the outliers

y.1=y[y$income<1000000, ]
boxplot(y.1$income, horizontal = T)

dim(y)
dim(y.1)

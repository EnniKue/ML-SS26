setwd("C:/Users/Hausdrache/Desktop/HAM/SS26/Machine Leaarning/ML-SS26")

# Untersuchung des Datensatzes

x=read.csv("student_dataset_6 Enni.csv")

View(x)
names(x)
str(x)

#Fehlende Werte 
#Aufgabe 1:

colSums(is.na(x))
md.pattern(x)
install.packages("tidyverse")
install.packages("mice")

library("tidyverse")
library("mice")
md.pattern(x)

mean(x$age, na.rm=T)
mean(x$income, na.rm=T)
mean(x$score, na.rm=T)

sd(x$income, na.rm=T)
sd(x$age, na.rm=T)
sd(x$score, na.rm=T)

dm.na=is.na(x$age)
table(dm.na)

View(dm.na)

chisq.test(x$gender, dm.na)

di.na=is.na(x$income)
table(di.na)

View(di.na)

chisq.test(x$gender, di.na)

ds.na=is.na(x$score)
table(ds.na)

chisq.test(x$gender, ds.na)

dm.na=is.na(x$age)
table(dm.na)

chisq.test(x$city, dm.na)

di.na=is.na(x$income)
table(di.na)

chisq.test(x$city, di.na)

ds.na=is.na(x$score)
table(ds.na)

chisq.test(x$city, ds.na)

dm.na=is.na(x$age)
table(dm.na)

chisq.test(x$department, dm.na)

di.na=is.na(x$income)
table(di.na)

chisq.test(x$department, di.na)

ds.na=is.na(x$score)
table(ds.na)

chisq.test(x$department, ds.na)

summary(x)


dm.ber= drop_na(x)

View(dm.ber)
summary(dm.ber)


x.fill=mice(x)
y=complete(x.fill)

colSums(is.na(y))

View(y)

summary(y)

sum(is.na(x$gender))

#Ausreißer
#Score

names(dm.ber)

boxplot(dm.ber$score, horizontal = T)
boxplot.stats(dm.ber$score)

density(dm.ber$score)
plot(density(dm.ber$score))

z_scores= scale(dm.ber$score)

# Werte finden, die absolut größer als 3 sind (Normalverteilung)
outliers_z = dm.ber$score[abs(z_scores) > 3]

# Falls keine > 3 existieren, kannst du zum Testen 2 nehmen
print(outliers_z)

#Income

boxplot(dm.ber$income, horizontal = T)
boxplot.stats(dm.ber$income)

density(dm.ber$income)
plot(density(dm.ber$income))

z_scores= scale(dm.ber$income)
outliers_z = dm.ber$income[abs(z_scores) > 3]
print(outliers_z)

#Age

boxplot(dm.ber$age, horizontal = T)
boxplot.stats(dm.ber$age)

density(dm.ber$age)
plot(density(dm.ber$age))

z_scores= scale(dm.ber$age)
outliers_z = dm.ber$age[abs(z_scores) > 3]
print(outliers_z)

# Tabelle ohne Ausreißer

no.outliers= filter(dm.ber, if_all(where(is.numeric), ~ abs(as.vector(scale(.))) < 3))
View(no.outliers)
summary(no.outliers)


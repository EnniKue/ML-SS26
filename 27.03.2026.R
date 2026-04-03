## Kommentare schreiben: SS26 Machine learning R Programmiing Intro
#1. Variables > data storage in R

x=c(0,1,-4,3.5,10)
length(x)

#2. Data Types> numeric, character, logic
class(x)

g=c("A", "B", "C", "A")
class(g)

L=c(T, F, T, F, T, F)
class(L)

#3. Data Structures> Vector, Matrix, dataframe

#consider ALL 5 employees of Bäckerei. And collect data on their 
#Gender, Salary(Monthly-M), Working hrs

#Vector (storage fpr data)
Gender=c("f", "m", "f", "m", "f")
Salary=c(2,2.5,3,2.8,1.8)
Working.Hrs=c(7,8,9,7,5)

#Matrix
m1=matrix(Salary, nrow = 5)
m1
m2=matrix(Gender, nrow = 5)
m2

dim(m1)

m3=cbind(m2, m1)
m3

str(m3)

#Dataframe
dt1=data.frame(GND=Gender, 
               SLR=Salary,
               WRK=Working.Hrs)
dt1

head(dt1, 3)
tail(dt1, 2)
View(dt1)

colnames(dt1)=c("Gender", "Salary", "WRK")
head(dt1)
dim(dt1)

# (), [], {}
dt1[1,3]
dt1[5,]
dt1[,2]

#Thee basic priciples in R programming
#1 Sequencing the codes are execited line by line TOP -> BOTTOM
#2 Selection, if else
t=6
if(t>0){
  print ("t is positive")
}else {
  print ("t is non positive")
}

#3 Loops, for
for (i in 1:8){
  print (i*i)
}

# take Gender and transform to a 0/1 variable s.t female:=0, and male:=1

Gender
new.Gender=c()

for (i in 1:5){
  if (Gender[i]=="f"){
    new.Gender [i]=0
  }else {
    new.Gender [i]=1
  }
}
new.Gender
Gender

## Consider Salary and Gender, add a bunus of 100/1000=0.1 Euros to the monthly salary
#if the Gender ist Female


Gender
Salary
new.Salary=c()

for (i in 1:length(Gender)){
  if (Gender[i]=="f"){
    new.Salary [i]=Salary[i]*1.1
  }else {
    new.Salary [i]=Salary[i]
  }
}
new.Salary
Gender

dt2=data.frame(G=Gender, 
               n.S=new.Salary)
dt2

View(dt2)

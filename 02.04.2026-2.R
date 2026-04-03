# Writing a function in R
Gehalt=c(3.5,4,5,5.5,2.8)

n.Gehalt=c()

for (i in 1:length(Gehalt)){
  if (Gehalt[i] >=4){
    n.Gehalt [i]=Gehalt[i]*0.9
  }else {
  n.Gehalt[i]=Gehalt[i]*0.95
  }
}
n.Gehalt
# muss noch machen
Charity.Contibution

##Aufgabe erklärt

x=c(2,3,4,5,10,1)
mean (X)

#writ a function that finds the sum and product of two numbers: x, y
# erst Funktionsname (hier f1)
f1=function(x,y){
  s=x+y
  p=x*y
  
  return(c(s,p))
}
f1(2,10)

#write an function that checks whether a qadratic equation has real solutions
#ax^2+bx+c=0
#delta= b^2-4ac
#if delta >=0 then real solutions
#(delta<0) no real solutions
f2=funcion(a, b, c) {
  delta =b^2 - 4*a*c
  
  if (delta >=0) {
    return("Real Soluition(s)")
  }
  }else{
    return("No real solution")
  }
}

# --- Examples ---
# Example 1: x^2 + 5x + 6 = 0 (delta = 25 - 24 = 1) -> Real
f2(1,-10,100)

# Example 2: x^2 + 4x + 4 = 0 (delta = 16 - 16 = 0) -> Real
print(f1(1, 4, 4))

# Example 3: x^2 + 2x + 5 = 0 (delta = 4 - 20 = -16) -> No real
print(f1(1, 2, 5))




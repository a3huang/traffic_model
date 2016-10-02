a=createBMLGridnew(5,7,c(10,10))
length(Which(a==1))
length(Which(a==2))
b=moveBlue(a)
c=moveBlueC(a)
d=moveRed(a)
e=moveRedC(a)
all(d==e)

a=createBMLGridnew(4,3,c(3,3))
d=moveRed(a)
e=moveRedC(a)
all(d==e)
length(Which(a==1))
length(Which(a==2))
b=moveBlue(a)
c=moveBlueC(a)

a=createBMLGridnew(7,10,c(15,10))
d=moveRed(a)
e=moveRedC(a)
all(d==e)
b=moveBlue(a)
c=moveBlueC(a)
all(b==c)

a=createBMLGridnew(9,13,c(30,30))
d=moveRed(a)
e=moveRedC(a)
all(d==e)
b=moveBlue(a)
c=moveBlueC(a)
all(b==c)

# there might be discrepancies since moveRed move ALL cars at once while
# moveRedC move cars individually one at a time

setwd("~/R/242/242assign2")
#source("oldfunctions.R")
dyn.load("moveCars")
source("grid.R")
source("gridC.R")

# a=createBMLGridnew(1000,1000,c(35000,35000))
a=createBMLGridnew(100,100,c(3500,3500))

a=createBMLGridnew(10,10,c(5,5))
all(runBMLGrid(a,0)==a)
all(runBMLGrid(a,1)==moveBlueC(a))
all(runBMLGridC(a,1)==runBMLGridC(a,2))
all(runBMLGridC(a,2)==moveRedC(moveBlueC(a)))
all(runBMLGridC(a,3)==moveBlueC(moveRedC(moveBlueC(a)))) # false
all(runBMLGridC(a,3)==runBMLGridnew(a,3))
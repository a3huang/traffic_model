createBMLGridnew=function(r,c,ncars){
  a=matrix(0,c,r)
  n=sum(ncars)
  location=sample(c*r,n)
  a[location[1:ncars[1]]]=1
  a[location[(ncars[1]+1):n]]=2
  class(a)=c("bml","matrix")
  a
}

moveBlueC=function(a){
  blue=which(a == 2, arr.ind=TRUE)
  m=.C("movebluecars",as.integer(a),nrow(a),ncol(a),blue,nrow(blue))[[1]]
  mat=matrix(m,nrow(a),ncol(a))
  class(mat) = c("bml","matrix")
  mat
}

moveRedC=function(a){
  red=which(a == 1, arr.ind=TRUE)
  m=.C("moveredcars",as.integer(a),nrow(a),red,nrow(red))[[1]]
  mat=matrix(m,nrow(a),ncol(a))
  class(mat) = c("bml","matrix")
  mat
}

runBMLGridnew=function(a,numSteps){
  if(numSteps == 0){
    return(a)
  }
  if(numSteps == 1){
    a=moveBlueC(a)
    # if numSteps is odd, end on a blue car
  } else if(numSteps %% 2 != 0){
    for(i in 1:((numSteps-1)/2)){
      a=moveBlueC(a)
      a=moveRedC(a)
    }
    a=moveBlueC(a)
    # if numSteps is even, end on a red car
  } else if (numSteps %% 2 == 0){
    for(i in 1:(numSteps/2)){
      a=moveBlueC(a)
      a=moveRedC(a)
    }
  }
  a
}

# this new function is slightly different from runBMLGridnew
# it handles 2 or more cars in a row differently, but otherwise is valid
runBMLGridC=function(a,numSteps){
  red=which(a == 1, arr.ind=TRUE)
  blue=which(a == 2, arr.ind=TRUE)
  m=.C("runGrid",as.integer(a),nrow(a),ncol(a),blue,nrow(blue),red,nrow(red),
     as.integer(numSteps))[[1]]
  mat=matrix(m,nrow(a),ncol(a))
  class(mat) = c("bml","matrix")
  mat
}

# 100 x 100
# a=createBMLGridnew(100,100,c(3500,3500))
# .3 seems to be freeflow
# .304 seems to be freeflow
# .305 seems to be freeflow
# .306 seems to be jammed <--- ### turning point
# .31 is already jammed

# 79 x 127
# a=createBMLGridnew(79,127,c(1504,1504)) ; .3
# a=createBMLGridnew(79,127,c(1454,1454)) ; .29
# a=createBMLGridnew(79,127,c(1479,1479)) ; .295
# a=createBMLGridnew(79,127,c(1484,1484)) ; .296
# a=createBMLGridnew(79,127,c(1489,1489)) ; .297
# a=createBMLGridnew(79,127,c(1494,1494)) ; .298
# a=createBMLGridnew(79,127,c(1499,1499)) ; .299
# .29 freeflow
# .295 freeflow
# .296 freeflow
# .297 freeflow
# .298 freeflow
# .299 freeflow
# .3 jammed <--- ### turning point

# bmlgridC much faster than bmlgridnew for large iterations
### function to randomly place cars on BML Grid at time t = 0 ###
createBMLGrid = function(r,c,ncars) {
  
  # want grid of r rows and c columns ; so create c x r matrix
  # represent blank spaces as 0, red cars as 1, and blue cars as 2
  a=matrix(0,c,r)
  rx=sample(c,ncars[1],replace=TRUE)
  ry=sample(r,ncars[1],replace=TRUE)
  bx=sample(c,ncars[2],replace=TRUE)
  by=sample(r,ncars[2],replace=TRUE)
  r=cbind(rx,ry)
  b=cbind(bx,by)
  
  # find all cars with duplicated coordinates
  m=rbind(r,b)
  overlap=which(duplicated(m)) 
  rdup=0
  bdup=0
  
  # remove cars with duplicated coordinates
  if(length(overlap)>0){
    
    # separate into red duplicates and blue duplicates
    over=split(overlap, overlap <= ncars[1])
    if(length(over)==1 & all(over[[1]] <= ncars[1])){
      rnew=r[-over[[1]],]
      bnew=b
      rdup=length(over[[1]])
    } else if(length(over)==1 & all(over[[1]] > ncars[1])){
      bnew=b[-(over[[1]]-ncars[1]),]
      rnew=r
      bdup=length(over[[1]])
    } else if(length(over)>1){
      rnew=r[-over[[2]],]
      bnew=b[-(over[[1]]-ncars[1]),]
      rdup=length(over[[2]])
      bdup=length(over[[1]])
    }
  } else {
    rnew=r
    bnew=b
  }
  
  # fill in red cars that are NOT duplicated
  if(length(rnew)==2) {
    a[rnew[1],rnew[2]]=1
  } else {
    for(i in 1:nrow(rnew)){a[rnew[i,1],rnew[i,2]]=1}
  }
  
  # fill in blue cars that are NOT duplicated
  if(length(bnew)==2) {
    a[bnew[1],bnew[2]]=2
  } else {
    for(i in 1:nrow(bnew)){a[bnew[i,1],bnew[i,2]]=2}
  }
  
  # find empty spaces to place duplicated cars and shuffle them
  open=which(a==0,arr.ind=TRUE)
  c1=sample(open[,1])
  c2=sample(open[,2])
  c=cbind(c1,c2)
  
  # place duplicated red cars in random empty space
  if(rdup > 0){
    for(i in 1:rdup){
      a[c[i,1],c[i,2]]=1
    }
  }
  
  # place duplicated blue cars in random empty space
  if(bdup > 0){
    for(i in (1+rdup):(bdup+rdup)){
      a[c[i,1],c[i,2]]=2
    }
  }
  
  class(a)=c("bml","matrix")
  a
}

### function to plot a graphical representation of BML Grid ###
plot.bml=function(a,...){
  image(a,col=c("white","red","blue"),...)
}

### function to move all blue cars up ###
moveBlue=function(a){
  lg=which(a[,ncol(a)]==2 & a[,1]==0)
  l=sapply(1:(ncol(a)-1),function(i) which(a[,i]==2 & a[,i+1]==0))
  
  if(length(lg)>0){
    a[lg,ncol(a)]=0
    a[lg,1]=2
  }
  
  for(i in 1:length(l)){
    a[l[[i]],i]=0
    a[l[[i]],i+1]=2
  }
  a
}

### function to move all red cars to the right ###
moveRed=function(a){
  lg=which(a[nrow(a),]==1 & a[1,]==0)
  l=sapply(1:(nrow(a)-1),function(i) which(a[i,]==1 & a[i+1,]==0))
  
  if(length(lg)>0){
    a[nrow(a),lg]=0
    a[1,lg]=1
  }
  
  for(i in 1:length(l)){
    a[i,l[[i]]]=0
    a[i+1,l[[i]]]=1
  }
  a
}

### function to produce BML Grid at time t = numSteps ###
runBMLGrid=function(a,numSteps){
  if(numSteps == 0){
    return(a)
  }
  if(numSteps == 1){
    a=moveBlue(a)
    # if numSteps is odd, end on a blue car
  } else if(numSteps %% 2 != 0){
    for(i in 1:((numSteps-1)/2)){
      a=moveBlue(a)
      a=moveRed(a)
    }
    a=moveBlue(a)
    # if numSteps is even, end on a red car
  } else if (numSteps %% 2 == 0){
    for(i in 1:(numSteps/2)){
      a=moveBlue(a)
      a=moveRed(a)
    }
  }
  a
}

### function that gives summary of grid at time t = numSteps ###
summary.bml=function(a,numSteps){
  
  if(numSteps==0){
    return(list(cars_moved=0,cars_blocked=0,average_velocity=0))
  }
  moved=length(which(runBMLGrid(a,numSteps-1) != runBMLGrid(a,numSteps)))
  
  if(numSteps %% 2 == 0){
    blocked=length(which(a==1)) - moved
    velocity=moved/length(which(a==1))
  }
  if(numSteps %% 2 != 0){
    blocked=length(which(a==2)) - moved
    velocity=moved/length(which(a==2))
  }
  
  list(cars_moved=moved,cars_blocked=blocked,average_velocity=velocity)
}
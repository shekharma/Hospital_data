#makeCacheMatrix function takes matrix as an argument.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL                                ##gives inv value as NULL  
  }
  get<-function()x                            ##get function with no argument print the assigned matrix
  setInverse<-function(inverse)inv<<-inverse
  getInverse<<-function()inv                  ##before going in cacheSolve this function print NULL 
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}

##cacheSolve function takes matrix as argument further calculate INVERSE then check whether determinant it is 0 or not by using if loop,further it passes to calculate 
##the INVERSE and then and then print out.

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){            ##checks determinant=0 or!=0.
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()                  ##given matrix assigns to mat variable.
  inv<-solve(mat,...)           ##this solve function calculate it's INVERSE.
  x$setInverse(inv)             
  inv                           ##print the INVERSE of given matrix.
}

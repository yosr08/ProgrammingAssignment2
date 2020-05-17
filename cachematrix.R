## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  #method to set the matrix
  set<-function(matrix){
    x<<-matrix
    inv<<-NULL
  }
  #method to get the matrix
  get<-function(){
    x
  }
  #meth to set the inverse of the matrix
  setInverse<-function(inverse){
    inv<<-inverse
  }
  #meth to get the inverse of the matrix
  getInverse <- function(){
    inv
  }
  list(set=set,get=get,setInverse=setInverse,getInverse)
}


## if the inverse has already been calculated the cachesolve will retreive the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     X<-x$getInverse()
     #return the inverse if it s already calculated
     if(!is.null(X)){
       message("get cached data")
       return(X)
     }
     #get the matrix from the object
     mat<-x$get()
     #calculate the inverse using matrix multip
     X<-solve(mat)%*% mat()
     #set the inverse to our object
     x$setInverse(X)
     #return the matrix
     X
}

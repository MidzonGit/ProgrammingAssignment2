## The makeCacheMatrix returns a special matrx object, a list
## of functions which can set and get the matrix passed to 
## the function as well as set and get their inverse. These 
## functions caches the value of inverse in the global environment

## For setting and returning the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMatrix<-function(y){
    x<<-y
    inv<<-NULL
  }
  getMatrix<-function() x
  setInverse<-function(solve) inv<<- solve
  getInverse<-function() inv
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInverse=setInverse, getInverse=getInverse)
}



## If inverse is already cached, directly returns the 
## cached value. Else, calculates the inverse and caches it

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$getMatrix()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}

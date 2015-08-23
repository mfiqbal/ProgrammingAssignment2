## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL 
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function () x
  setInverse <- function(inverseM) im <<- inverseM
  getInverse <- function() im
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Return a matrix that is inverse of x
cacheSolve <- function(x, ...) {
        im <- x$getInverse()
        if(!is.null(im)){
          message ("getting the cached inverse matrix")
          return(im)
        }
        ##library MASS needed to use the function ginv
        library(MASS)
        matx <- x$get()
        im <- ginv(matx)
        x$setInverse(im)
        im
        
}

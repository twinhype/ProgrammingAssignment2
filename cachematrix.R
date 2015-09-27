## These two functions, makeCacheMatrix and cacheSolve, compute the inverse of
## a matrix. makeCacheMatrix serves to store the matrix and its inverse. 
## cacheSolve actually computes the inverse if it had not been calculated before, 
## otherwise it simply returns the previously computed value.

## This function creates a special "matrix" object that can be cached and returned 

makeCacheMatrix <- function(x = matrix()) {
  ## variable to store the inverse of the matrix
  invm <- NULL
  
  ## set the matrix to a new value
  setm <- function(y) {
    x <<- y
    invm <<- NULL
  }  
  
  ## return the matrix
  getm <- function() x
  
  ## store a value for the inverse
  setinv <- function(inv) invm <<- inv
  
  ## return the value of the inverse
  getinv <- function() invm
  
  list(setm= setm, 
       getm = getm,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix 
## and returns it, if the value returned by its getinv method is null, othewrwise
## it returns the previously computed (cached) value returned by getinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  
  ## if the value had been computed before, return it...
  if(!is.null(invm)) { 
    message("getting cached inverse")
    return (invm)
  }
  
  ## ... otherwise compute it, cache it and return it
  matr <- x$getm()
  invm <- solve(matr)
  x$setinv(invm)
  invm
}

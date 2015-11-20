## Put comments here that give an overall description of what your
## functions do
##
## Two main functions : makeCacheMatrix() and cacheSolve()
## The makeCaceheMatrix() is used to create a "special matrix" that can be used to cache inverse of a matrix.
## It consists within it 4 sub-functions, each performing different tasks on the matrix A (see below description) 
##
## CacheSolve() computes or load from the cached A, 
## the inverse of matrix x parsed to it by makeCacheMatrix.
##

## Write a short comment describing this function

##
## The makeCaceheMatrix() function takes a matrix x as input.
## It first initialize the inverse matrix A to NULL.
##
## It consists within it 4 functions setx(), getx(), setInverse() and 
## getInverse().
## setx() takes an input and sets the matrix x to the input value, 
##   and resets A to NULL
## getx() gets the value of the matrix x
## setInverse() takes an input and set the inverse matrix to the input value 
## getInverse() gets the value of the inverse matrix A
##

makeCacheMatrix <- function(x = matrix()) {
  A <- NULL
  
  setx <- function(y=matrix())
  {
    x <<- y
    A <<- NULL
  }
  
  getx <- function() x
  
  setInverse <- function(InvMat = matrix()) A <<- InvMat
  getInverse <- function() A
  
  list(setx = setx, getx = getx,
       setInverse = setInverse,
       getInverse = getInverse)
  
}




## Write a short comment describing this function
## 
## cacheSolve() first checks if the inverse matrix A has been cached 
## using the getInverse() function within makeCacheMatrix().
## 
## If A has been computed, i.e. A is not NULL, it returns the matrix A
## and display a message "getting cached data".
## 
## If A has not been cached, A = NULL, then CacheSolve 
## takes the input matrix x and computes the inverse and cache it
## using the setInverse() function within the makeCacheMatrix().
##


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  A <- x$getInverse()
  
  if(!is.null(A)) {
    message("getting cached data")
    return(A)
  }
  
  B <- x$getx()
  A <- solve(B, ...)
  x$setInverse(A)
  A
}


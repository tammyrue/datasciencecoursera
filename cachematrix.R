## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## x is an invertible square matrix
## 1.This function returns: a list containing functions to
##              1.1. set the matrix
##              1.2. get the matrix
##              1.3. set the inverse
##              1.4 get the inverse
##    then, the list becomes the input to cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    
  inv = NULL
  set = function(y) {
    x <<- y # The use of <<-` assign a value to matrix x in a different enviroment, not the current.
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Return : a matrix that is the inverse of 'x'
## which is the original matrix used as input to makeCacheMatrix() 
## x is the output of makeCacheMatrix() for this function

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  # When the inverse was already calculated 
  if (!is.null(inv)){
  # Then get it from the cache and dont make calculations. 
    message("Obtained from cached data")
    return(inv)
  }
  
  # If the inverse wasnt calculated already, do so: 
  matrix.info = x$get()
  inv = solve(matrix.info, ...)
  
  # sets the value of the inverse in the cache.
  x$setinv(inv)
  
  return(inv)
  
  }
# to test this code both functions must work together
# if run separately, makeCacheMatrix will show you the list of functions numbered before.

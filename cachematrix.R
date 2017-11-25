## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Using both functions:
##if cache inverse = available
##       then cacheSolve retrieves it
##       else it computes, cache an returns it

makeCacheMatrix <- function(x = matrix()) {
##this function creates a special 
  ##"matrix" object that can cache its inverse
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)#solve -> computing the inverse of a square matrix
  x$setInverse(inv)
  inv
}

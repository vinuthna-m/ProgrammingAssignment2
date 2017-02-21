## Finding matrix inversion is an expensive computation. So if a matrix 
## inversion is performed once, then it is stored in the cache and when
## matrix inversion for the same matrix is called it is returned from 
## the cache instead of computing again

## creates a matrix object which can cache its inverse by returning a list
## which gets the matrix value, sets the matrix value, gets the inverse 
## and sets the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## If the inverse of current matrix is found in cache, then it prints a message
## getting cached data and returns the inverse, if inverse is not found in cache
## then inverse is calculated and stored in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  return(inv)
}


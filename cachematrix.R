## If a matrix inversion is performed once, then it is stored in the cache 
##and when matrix inversion for the same matrix is called it is returned from 
## the cache instead of computing again

## creates a matrix object which can cache its inverse 
## returns a list which contains functions to
##      get the matrix value
##      set the matrix value 
##      get the inverse 
##      set the inverse

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
  ## Store a matrix that is the inverse of 'x' if found in cache
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) # returns the inverse found in cache
  }
  #If inverse is not found in the cache, then
  data <- x$get() #Get the data
  inv <- solve(data) #Find the inverse using solve() function
  x$setInverse(inv) #Set the inverse value of matrix in cache
  return(inv) #return inverse of the matrix
}


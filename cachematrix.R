## The both functions are used to create an object
## that stores a matrix and caches its inverse

## The following function creates a matrix that can cache its inverse
## It contains a list of functions that allow to set the value of the matrix,
## to get the value of the matrix, to set the value of the inverse
## of this matrix and to get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The following function calculates inverse matrix. 
## At first it checks if the inverse has already been calculated. 
## If so, it gets value from cache and skips computation.
## Otherwise, it calculates the inverse and sets the value of it in the cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse  
}




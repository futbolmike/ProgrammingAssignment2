## R programming assignment 2 Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.
## Creates the object in the form of  list with `set` and `get` functions,
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## The inverse starts out as NULL
  ## By setting the object, I assign the new argument to be my stored value and
  ## reset my inverse calculation
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Returns my internal object
  get <- function() x
  ## Get and Set functions for the inverse, as above
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  ## Calculating the inverse
  x$setInverse(inv)
  inv
}

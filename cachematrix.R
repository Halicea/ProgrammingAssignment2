## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  if(is.null(x$getInverse())){
    print("constructing cache,first time")
    x$setInverse(solve(x$get(), ...))
  }
  x$getInverse()
}
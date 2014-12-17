
## returns a 'special' matrix with ability to cache its inverse
## the matrix is represented as a list with methods get, set, setInverse, getInverse
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

## Return a matrix that is the inverse of 'x' by laveraging the chache the object has
cacheSolve <- function(x, ...) {
  if(is.null(x$getInverse())){
    #calculate and save the inverse in the cache
    x$setInverse(solve(x$get(), ...))
  }
  #return the cached result
  x$getInverse()
}
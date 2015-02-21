## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there is some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. This file contains a pair of functions that cache the
## inverse of a matrix.

## Makes a special matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

## Calculates the inverse of a matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

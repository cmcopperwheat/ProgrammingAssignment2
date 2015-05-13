## This script contains two functions. makeCacheMatrix creates a special
## 'matrix' object which can cache its inverse. cacheSolve computes the
## inverse of the special 'matrix' returned by makeCacheMatrix.


## makeCacheMatrix creates a special 'matrix' which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(matrix) inverse <<- matrix
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created with 
##'makeCacheMatrix'. It first checks if the inverse has already been 
## calculated (and the matrix has not changed). If this is the case then the
## inverse is retrieved from the cache. If not, the computation is performed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

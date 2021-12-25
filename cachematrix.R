## Put comments here that give an overall description of what your
## functions do

## cache the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## initialize inv as NULL; will hold value of inverse matrix
  set <- function(y) { ## set to assign new function
    x <<- y ## value of matrix in parent environment
    inv <<- NULL ## if there is a new matrix, reset inv to NULL
  }
  get <- function() {x} ## define the get function - returns value of the matrix argument
  setInv <- function(inverse) {inv <<- inverse} ## assigns value of inv in parent environment
  getInv <- function() {inv} ## gets the value of inv where called
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Check if inverse on=f an invertible matrix is in the cache. If not, calculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) { ## if inverse has not been already cached, it is cached and collected
    message("cached data collected")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv}

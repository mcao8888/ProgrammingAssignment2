# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

###################################################################
## Purpose:      Creates a list containing functions to set/get the
##               value of the matrix and its inverse
##
## Input:  
##               x:  An invertible matrix
##
## Output:       a list containing these functions:
##               set:  set the value of the matrix
##               get:  get the value of the matrix
##               setinverse:  set the value of the inverse of the matrix
##               getinverse:  get the value of the inverse of the matrix
####################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


###################################################################
## Purpose:      Calculates the inverse of a matrix.
##               It uses cache to avoid computation if cache exists.
##               It sets the cache if none exists.
##
## Input:  
##               x:  An invertible matrix
##
## Output:       The inverse of the matrix
####################################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

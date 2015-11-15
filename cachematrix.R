## Put comments here that give an overall description of what your
## functions do

# These functions act to allow one to effectively cache the calculation 
# of the inverse of a matrix. This reduces computational time, as
# one does not need to compute the inverse of a given matrix more 
# than once. This can result in significant time improvements, 
# such as if the inverse of a matrix is calculated in a loop.

# Usage: Call makeCacheMatrix() on an invertible matrix and store 
#        the result. Calling cacheSolve() on this result will now
#        compute the inverse of the matrix and store it in the cache
#        if it has not been previously computed. If it been previously
#        computed, it will retrieve the result from the cache without
#        recalculating.


## Write a short comment describing this function
# makeCacheMatrix() creates a list that contains a function to
# set and get the value of the input matrix and to set and get
# the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
# cacheSolve() uses the output of makeCacheMatrix() to calculate
# the inverse of the input matrix if it has not already been
# calculated, and retrieves it from the cache otherwise. If it 
# calculates the inverse, it stores this in the cache, which 
# is done using the setinv() function.

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


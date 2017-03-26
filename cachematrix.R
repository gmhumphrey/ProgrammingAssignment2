## This pair of functions allows for more efficient code by caching unchanging elements for use throughout the program.

## This function creates a "special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInv <- function(inversion) mInv <<- inversion
  getInv <- function() mInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inversion of a matrix
## To optimize run times, it tests for the existence of a calculated inversion of a matrix.
## If one doesn't exist, it computes the inverse and writes that to the environment to which it was defined.

cacheSolve <- function(x, ...) {
  ##Test for existence of cached matrix
  mInv <- x$getInv()
  if(!is.null(mInv)) {
    message("Retrieving cached data.")
    return(mInv) ##Code returns cached matrix and ends here if TRUE
  }
  
  ##If not yet cached: 
  ##Intake the data from the special object
  data <- x$get()
  ##Compute the inverse of the matrix
  mInv <- solve(data, ...)
  ##Store the inverted matrix back to original object
  x$setInv(mInv)
  ##Return the newly calculated result, since the function must return a matrix either way
  mInv
}

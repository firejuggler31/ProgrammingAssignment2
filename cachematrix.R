## These functions are designed to invert a matrix and cache the matrix.
## If the inverse is asked for again, the cached value is returned, saving time.

## Make a list containing 4 functions: set the value of the matrix,
##  get the value of the matrix, set the inverse of the matrix, and 
##  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve,
      getsolve = getsolve)
}


## This function checks to see if the matrix has be inverted.  If so,
##  it returns said inverse, if not it solves for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}

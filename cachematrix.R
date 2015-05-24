## Collection of functions that can calculate the inverse of a matrix
## and also cache this value so the overhead of computation is only performed once

## Create object that can be used to get/set matrix data and calculate inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Function to return inverse of matrix and cache inverse so each subsequent call uses cached data
## If input data chances the inverse will be recalculated

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached solved data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

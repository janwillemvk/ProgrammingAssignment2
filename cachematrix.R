## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above

# makeCacheMatrix
# this function creates a special "matrix" object that can cache its inverse.
# This function assumes that the matrix supplied is always invertible.

makeCacheMatrix<- function(x = matrix()) {
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

## cacheSolve
# cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed)
# , then the cachesolve will retrieve the inverse from the cache.
#  This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
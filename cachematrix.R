## Two functions to avoid repeatedly redoing timeconsuming computation of 
# the inverse of a matrix.
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the matrix.

# details: makeCacheMatrix
# this function creates a special "matrix" object that can cache its inverse.
# This function assumes that the matrix supplied is always invertible.
# examples:  see below


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

## details: cacheSolve
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

# examples:
# source('cachematrix.R')
# A <- matrix(c(1,2,2,1), nrow=2, byrow=TRUE)
# c<-makeCacheMatrix(A)
# [,1] [,2]
# [1,]    1    2
# [2,]    2    1

# c$set(matrix(c(19,8,11,2,18,17,15,19,10), nrow = 3))
# c$get()
# [,1] [,2] [,3]
# [1,]   19    2   15
# [2,]    8   18   19
# [3,]   11   17   10
# c$getsolve()
# NULL
# amat <- matrix(c(19,8,11,2,18,17,15,19,10), nrow = 3)
# W <-makeCacheMatrix(amat)
# cacheSolve (W)
# [,1]         [,2]        [,3]
# [1,]  0.04219534 -0.069341989  0.06845677
# [2,] -0.03806433 -0.007376807  0.07111242
# [3,]  0.01829448  0.088816760 -0.09619357
# cacheSolve (W)
#getting cached data
# [,1]         [,2]        [,3]
# [1,]  0.04219534 -0.069341989  0.06845677
# [2,] -0.03806433 -0.007376807  0.07111242
# [3,]  0.01829448  0.088816760 -0.09619357
# W$get()
# [,1] [,2] [,3]
# [1,]   19    2   15
# [2,]    8   18   19
# [3,]   11   17   10
# c$get()
# [,1] [,2] [,3]
# [1,]   19    2   15
# [2,]    8   18   19
# [3,]   11   17   10
# c$setsolve(amat)
# c$getsolve()
# [,1] [,2] [,3]
# [1,]   19    2   15
# [2,]    8   18   19
# [3,]   11   17   10
# cacheSolve (c)
#getting cached data
# [,1] [,2] [,3]
# [1,]   19    2   15
# [2,]    8   18   19
# [3,]   11   17   10
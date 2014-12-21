# My solution to the Coursera course "R Programming", assignment 2:
# https://www.coursera.org/course/rprog
# 
# Trying to adhere to the "Google R Style Guide" for readability:
# http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#
# Assignment: caching the inverse of a matrix
# 
# Functions used:
# makeCacheMatrix:
#   set/get matrix value, set/get inverse matrix value
# cacheSolve:
#   calculate inverse matrix if not cached, or retrieve cached inversed matrix
#
# Assumptions: Matrix is always invertible (i.e. no further checks)


makeCacheMatrix <- function(x = matrix()) {
# This function caches a matrix:
#   set/get matrix value
#   set/get inverse matrix value
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
    setinverse = setinverse, 
    getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
# This function calculates the inverse matrix if it was not calculated and
# cached before. Otherwise, it will retrieve the cached inversed matrix.
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}

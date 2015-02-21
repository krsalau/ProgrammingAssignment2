# Author: Kehinde Rilwan Salau
#         kehinde.salau@gmail.com
#         krsalau@email.arizona.edu
# Created: Feb 21st, 2015
#
#Inspired by code written by Roger Peng on caching vector means 
#(https://class.coursera.org/rprog-011/human_grading/view/courses/973492/
#assessments/3/submissions)
#
#In this script, we Write the following functions:
#1. makeCacheMatrix: This function creates a special "matrix" object that can 
#cache its inverse.
#2. cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.
###############################################################################

#The first function, makeCacheMatrix creates a special "matrix", which 
#contains a function to
#1. set the matrix values
#2. get the matrix values
#3. set the inverse matrix values
#4. get the inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The following function calculates the inverse of the special "matrix" created 
#with the above function. However, it first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache and skips 
#the computation. Otherwise, it calculates the inverse of the matrix and sets 
#the inverse matrix values in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

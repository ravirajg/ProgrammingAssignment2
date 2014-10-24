## The following functions help you create a matrix object that can cache its own inverse and return it
## Assumptions -- Input matrix is a square invertible matrix
## Usage:
##
## source("cachematrix.R")
## p<-matrix(1:4,2,2)
## m<-makeCacheMatrix(p)
## cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Note: The first time cacheSolve is invoked the inverse for matrix is cached 
## and the second call returns the inverse from cache


## Function to create a special matrix object that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to return a matrix that is the inverse of the original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  x$setinverse()
  x$getinverse()
}

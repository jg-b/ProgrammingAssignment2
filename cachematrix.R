## cachematrix.R is comprised of two functions which enable the creation of a
## special matrix object which is able to calculate and store in the global
## environment its inverse, so that solve() needs not be called every time

## makeCacheMatrix creates a special "matrix" object which is able to
## cache its inverse in the form of a vector containing four functions:
##   set - stores matrix in x in the global environment
##   get - returns matrix 
##   setinverse - calculates inverse of matrix and stores in global environment
##   getinverse - returns matrix inverse from global environment
makeCacheMatrix <- function(x = matrix()) {
  #creates a special matrix object that is able to cache its inverse
  matrix_inverse <- NULL 
  set <- function(y = matrix()) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) matrix_inverse <<- solved
  getinverse <- function() matrix_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve returns a matrix which is the inverse of the special matrix
##in the form of a vector of 4 functions that it is passed, with the steps:
##  1) use the get inverse function to see if the matrix inverse has been calculated
##  2) if it has been calculated, return the inverse from the cache
##  3) otherwise, calculate the matrix inverse using solve() and set the cache
##     so that it does not need to be recalculated 
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
      message("getting cached data")
      return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}

##to test these functions, can execute the following code:
##source("cachematrix.R")
##original = matrix(c(7,2,1,0,3,-1,-3,4,-2), nrow=3, ncol=3) 
##specialmatrix <- makeCacheMatrix(original)
##cacheSolve(specialmatrix) #caclulates and returns inverse
##cacheSolve(specialmatrix) #retrieves inverse from memory

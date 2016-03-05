## These functions take a matrix as a parameter and calculate the inverse of the given matrix.
## The functions initially look for a cached value and return that value if available without 
## performing the calculation.  Otherwise, the calculation is performed and the inverse is returned.  

## makeCacheMatrix takes a matrix as the argument an returns a list of functions that are 
## used to capture and set both the value and inverse value of the matrix in conjunction with
## the following cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  getmatrix <- function()x
  
  setInverse <- function(Inv) Inverse <<- Inv
  
  getInverse <- function() Inverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix that was provided as the argument to 
## the makeCacheMatrix function above.  If the inverse value is already cached then it returns the 
## cached value and skips the calculation.  

cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()
  
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$getmatrix()
  Inverse <- solve(data)
  x$setInverse(Inverse)
  Inverse
}

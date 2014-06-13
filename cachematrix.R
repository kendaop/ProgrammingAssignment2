## Creates a 'makeCacheMatrix' object with built-in functions for getting and setting the values of a matrix and its inverse.
## Access methods include 'get()', 'set()', 'getInverse()', and 'setInverse()'.
makeCacheMatrix <- function(mat = matrix()) {  
  inv = NULL
  
  # Sets the value of the matrix, and clears any cached value for its inverse.
  set = function(x = matrix()) {
    mat <<- x
    inv <<- NULL
  }
  
  # Gets the value of the matrix.
  get = function() mat
  
  # Calculates the inverse of the matrix and caches the value.
  setInverse = function() inv <<- solve(mat)
  
  # Returns the inverse of the matrix, if calculated
  getInverse = function() inv
  
  # Returns the access methods.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse matrix of 'x'. Checks cache for the already-calculated value and returns it, if found. 
## Otherwise, calculates and caches the inverse matrix of 'x'.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getInverse())) {
    print("Using cached data...")
    return(x$getInverse())
  }
  
  ## Calculates and caches the inverse of 'x'. Returns the calculated matrix.
  print("Caching inverse matrix...")  
  print(x$setInverse())
}

## SAMPLE CODE USED FOR TESTING - 
##     Creates a matrix and finds its inverse. 
##     Then uses the inverse as the base matrix and finds its inverse (the original matrix).
##
## matrix = matrix(c(1,0,5,2,1,6,3,4,0), nrow=3)
## mobj = makeCacheMatrix(matrix)
## mobj$get()
## mobj$getInverse()
## cacheSolve(mobj)
## inverse = mobj$getInverse()
## 
## rm(mobj)
##
## mobj = makeCacheMatrix(inverse)
## mobj$get()
## mobj$getInverse()
## cacheSolve(mobj)
## mobj$getInverse()
#makecacheMatrix is a function that contains helper functions for getting and setting an input matrix and its inverse

## makecacheMatrix is an utility function  that contains a list of functions for returning a input matrix and it's inverse.
## It has a get and set function for the input matrix and its inverse matrix, that allows setting said matrices from other enviroment.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function will attempt to solve the inverse of a matrix that is passed in through the makeCacheMatrix utility function.
## makecacheMatrix already contains a variable holding the inverse of the matrix, which is either already calculated or null. 
## cacheSolve will use the i variable in makecachMatrix to return the inverse without calculating it, or if it's not set in makeCacheMatrix, 
## calculate it, set it in makeCacheMatrix and return it.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if (!is.null(i))
    {
      message("getting cached inverse of i")
      return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setInverse(i)
    i
}

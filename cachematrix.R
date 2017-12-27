## makeCacheMatrix creates a matrix and is capable of caching the
## inverse of the matrix. cacheSolve returns the cached inverse of
## the matrix or calculates the inverse and returns it it is not
## already cached.

## Creates a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <<- solve(x)
  x
}


## Returns a matrix that is the inverse of "x"
### If the inverse has been cached, it will be returned
### Otherwise, the inverse will be calculated

cacheSolve <- function(x, ...) {
  m <<- inverse
  ## Return a matrix that is the inverse of 'x' if it has been cached
  if(!is.null(inverse)) {
    return(inverse)
  }
  ## Calculates and returns the inverse matrix if it has not been cached
  inverse <<- solve(x)
  inverse
}
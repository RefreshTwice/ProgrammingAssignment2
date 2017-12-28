## makeCacheMatrix creates a matrix and is capable of caching the
## inverse of the matrix. cacheSolve returns the cached inverse of
## the matrix or calculates the inverse and returns it it is not
## already cached.

## Creates a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Initializes x and m
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x
  ##Setup for cacheSolve to input a value for m once calculated
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ##Lists the functions with names to be referenced in cacheSolve
  list(setmat = setmat, getmat = getmat,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of "x"
### If the inverse has been cached, it will be returned
### Otherwise, the inverse will be calculated

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## Return a matrix that is the inverse of 'x' if it has been cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Calculates and returns the inverse matrix if it has not been cached
  data <- x$getmat()
  m <- solve(data)
  x$setinverse(m)
  m
}
## These functions allow you to create a matrix and cache it's inverse. 
## If you try to calculate the inverse of a matrix again, this will retrive the value from the cached inverse.

## This function creates a special list object containing a matrix with the ability to cache its inverse

makeCacheMatrix <- function(x = matrix(1,1,1))
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of a matrix as defined by the makeCacheMatrix function above.
## If the inverse of the matrix has already been calculated then retrieve the inverse, otherwise calculate it

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
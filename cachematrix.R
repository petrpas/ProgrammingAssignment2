

## Function makeCacheMatrix creates a list object containing the matrix
## and its initially empty cached inverse matrix
## Parameter x: a square matrix
## Return the cache matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function solves the inverse matrix and stores the result into
## the inner structure of the x.
## Parameter x: the cache matrix object
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
}

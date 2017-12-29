## The purpose of the functions are to cache potentially time consuming computations
## for later retreival.  The functions below are based on the example functions.

## This takes the matrix and sets two global variables: x and a.  It then creates a list with the required
## functions that can be re-read by cachesolve.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will determine if the inverse has already been computed by checking variable a
## if variable a is not null, it will return the cached variable.  Otherwise it will compute
## the inverse of the matrix and set its value in the list for later retrieval

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  message("computing matrix inverse")
  a <- solve(data, ...)
  x$setinverse(a)
  a
}



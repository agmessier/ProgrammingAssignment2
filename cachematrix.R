## These functions calculate the inverse of a matrix, and cache the inverse once it is computed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   if(!is.null(x)) {
      dims <- dim(x)
      if(dims[1] != dims[2]) {
         message("Matrix is not square.  Is not invertable")
      }
   }
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
      dims <- dim(y)
      if(dims[1] != dims[2]) {
         message("Matrix is not square.  Is not invertable")
      }
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## regrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}


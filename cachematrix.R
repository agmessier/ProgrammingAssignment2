## These functions calculate the inverse of a matrix, and cache the inverse
## once it is computed.

## This function creates a special "matrix" object that can cache its inverse.
## It implements set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
   # notify user that he may get unexpected results if matrix is not square
   if(!is.null(x)) {
      dims <- dim(x)
      if(dims[1] != dims[2]) {
         message("Matrix is not square.  It is not invertible")
      }
   }
   i <- NULL # invalidate inverse before it is set

   # sets x to argment value and invalidates inverse
   set <- function(y) {
      x <<- y
      i <<- NULL
      dims <- dim(y)
      if(dims[1] != dims[2]) {
         message("Matrix is not square.  It is not invertible")
      }
   }

   # returns matrix
   get <- function() x
   
   # assigns value to inverse (doesn't perform inversion)
   setinverse <- function(inverse) i <<- inverse
   
   #returns inverse
   getinverse <- function() i
   
   # return list of available functions.
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
   # get cached value of i
   i <- x$getinverse()
   
   # if i has a value, return i
   if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
   }
   
   #otherwise, compute i and cache its value
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   
   # return i
   i
}


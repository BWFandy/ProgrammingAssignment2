## 2 Functions that cache the inverse of a matrix:

## This one creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
     
     ## Assign something for the inverse property
     i <- NULL
     
     ## create a matrix
     set <- function( matrix ) {
          m <<- matrix
          i <<- NULL
     }
     
     ## get the matrix
     get <- function() {
          m
     }
     
     ## set the inverse of the matrix
     setInverse <- function(inverse) {
          i <<- inverse
     }
     
     ## get the inverse of the matrix
     getInverse <- function() {
          ## Return the inverse property
          i
     }
     
     ## Returns a list of the methods
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## Compute the inverse of the special matrix returned above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     
     ## Return the inverse if its already set
     if( !is.null(m) ) {
          message("getting cached data")
          return(m)
     }
     
     ## Get the matrix from our object
     data <- x$get()
     
     ## Calculate the inverse using matrix multiplication
     m <- solve(data) %*% data
     
     ## Set the inverse to the object
     x$setInverse(m)
     
     ## Return the matrix
     m
} 

## end

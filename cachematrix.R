## Compute and cache inverse of matrix. If inverse 
## already computed, read from cache.
##

## Matrix function to cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## Computes inverse of matrix.  Check cache. If inverse
## has already been computed, retrieve from cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached inverted matrix")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinverse(m)
     m
}

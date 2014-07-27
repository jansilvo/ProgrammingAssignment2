## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## These functions cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## Compute the inverse of a matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # If the inverse has already been calculated, retrieve it from the cache.
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    # else calculate the inverse
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)

    inv
}

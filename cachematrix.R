## Matrix inversion can be a costly computation, and in some situations it may 
## be benificial to cache the inverse of a matrix rather than compute it 
## repeatedly. The following pair of functions provide this functionality. This
## code is a straightforward modification of the example code for caching the 
## mean of a vector given in the assignment description.

## This function extends the "matrix" object with getters and setters that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix using the extended "matrix"
## object returned by makeCacheMatrix. The cacheSolve function will retrieve
## the cached inverse if it has already been calculated and the matrix has not
## changed.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
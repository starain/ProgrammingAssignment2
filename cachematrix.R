## Calcates inverse for a matrix. Returns cached result if the inverse has
## already been calculated (and the matrix has not changed).

## makeCacheMatrix returns an instance of "CacheMatrix" object, which has
## functions for catching its inverse:
##   set(x): sets the matrix object (and reset cached inverse).
##   get(): returns the matrix object.
##   setinverse(inv): sets cached inverse.
##   getinverse(): returns cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes and returns the inverse from given CacheMatrix object.
## If the inverse has already been calculated (and the matrix has not changed),
## then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv = x$getinverse()
    if (!is.null(inv)) {
        ## Found inverse from the cache, return directly.
        message("getting cached data")
        return(inv)
    }
    ## No inverse from the cache, calculate and cache the result.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## makeCacheMatrix and cacheSolve functions provide a framework for
## caching the inverse of matrix

## makeCacheMatrix 
##   This function creates a special "matrix" object that can cache 
##   its inverse.

makeCacheMatrix <- function(x = numeric()) {
    invrs <- NULL   # invrs is cached inverse value
    set <- function(y) {
        x <<- y
        invrs <<- NULL   # initialize invrs, since matrix was reset
    }
    get <- function() x
    setinvrs <- function(inverse) invrs <<- inverse    #save inverse value
    getinvrs <- function() invrs
    list(set = set, get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)
}

## cacheSolve 
##    This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. 
##    If inverse was calculated once, function will return cached value
cacheSolve <- function(x, ...) {
    invrs <- x$getinvrs()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinvrs(invrs)
    invrs
}
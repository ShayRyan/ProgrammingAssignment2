## This pair of functions implement the caching of a matrix inversion
## to avoid repeating the inversion calculation for the same matrix and thus
## save processing time.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # inverse is initially set to NULL
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # return the original vector x
    get <- function() {
        x
    }
    
    # called by cacheSolve once. Inverse matrix stored in inv
    setinv <- function(solve) {
        inv <<- solve
    }
    
    # return the cached inverse matrix to cacheSolve
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of a special matrix object returned
## by the'makeCacheMatrix' function. However, if the inverse has already been
## calcuated for an unchanged matrix, then cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

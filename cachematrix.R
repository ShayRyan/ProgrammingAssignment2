## Put comments here that give an overall description of what your

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$setinv(inv)
    inv
}

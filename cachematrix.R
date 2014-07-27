## These functions compute and cache the inverse of a square matrix the first 
## time, in order to look it up in the cache if we need it again, instead of recomputing it.

## This function creates a cache matrix. It can: set the value of the matrix, get it,
## set the value of its inverse and get it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function is used to get the inverse of a given matrix. It first checks the 
## cache to see if the inverse has already been calculated. If so, it returns its value.
## Otherwise, the inverse is computed then cached for a future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix inv that is the inverse of 'x'
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

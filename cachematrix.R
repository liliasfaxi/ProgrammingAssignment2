## These functions compute and cache the inverse of a square matrix the first 
## time, in order to look it up in the cache if we need it again, instead of recomputing it.

## This function creates a cache matrix. It can: set the value of the matrix, get it,
## set the value of its inverse and get it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # setting the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # getting the value of the matrix
        get <- function() x
        # setting the value of the inverse of the matrix
        setinv <- function(solve) inv <<- solve
        # getting the value of the inverse of the matrix
        getinv <- function() inv
        
        # the list to be returned
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function is used to get the inverse of a given matrix. It first checks the 
## cache to see if the inverse has already been calculated. If so, it returns its value.
## Otherwise, the inverse is computed then cached for a future use.

cacheSolve <- function(x, ...) {
        # search for the inverse of the matrix 'x' in the cache
        inv <- x$getinv()
        # if the inverse of the matrix is in the cache, return its value 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if not, compute it again...
        data <- x$get()
        inv <- solve(data, ...)
        # ... store it in the cache...
        x$setinv(inv)
        # ... then return its value.
        inv
}

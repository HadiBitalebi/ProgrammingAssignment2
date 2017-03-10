## This document is for chaching a matrix inverse

## This funtion create a list of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        Invrs <- NULL
        set <- function(y) {
                x <<- y
                Invrs <<- NULL
        }
        get <- function() x
        setInvrs <- function(solve) Invrs <<- solve
        getInvrs <- function() Invrs
        list(set = set, get = get,
             setInvrs = setInvrs,
             getInvrs = getInvrs)
}


## Check if the inverse of a matrix is cached thehb return from cach, 

cacheSolve <- function(x, ...) {
        Invrs <- x$getInvrs()
        if(!is.null(Invrs)) {
                message("getting cached data")
                return(Invrs)
        }
        data <- x$get()
        Invrs <- solve(data, ...)
        x$setInvrs(Invrs)
        Invrs
}

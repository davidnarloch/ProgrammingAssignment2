## These two functions in combination are able to reduce time-consuming
## computations, related to calculating the inverse of a matrix.
## They prevent R from computing the inverse a second time, if the inverse has
## already been computed before.
## This can be especially useful in loops.
## The value of the inverse is cached and if needed again it can be looked up
## instead of having to recompute it.


## This function creates a list, which contains the four functions "set",
## "get", "setinverse" and "getinverse" which are defined inside the
## makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the given matrix that was created with
## the makeCachMatrix function.
## If the inverse has already been calculated, it gets the inverse from the
## cache, otherwise it computes the inverse of the matrix and sets that value
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
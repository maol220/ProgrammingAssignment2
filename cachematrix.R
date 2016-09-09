## The following two functions, makeCacheMatrix and cacheSolve, will cache the
## inverse of a matrix and will retrieve this inverse from the cache, instead of
## computing for it again, provided that the matrix has not changed.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function above.
## If the matrix has not changed and its inverse had already been calculated,
## then the cacheSolve function would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrx <- x$get()
        inv <- solve(matrx, ...)
        x$setInverse(inv)
        inv
}

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. This assignment is to write a pair 
## of functions that cache the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
        IM <- NULL
        set <- function(Y) {
                X <<- Y
                IM <<- NULL
        }
        get <- function() X
        setsolve <- function(solve) IM <<- solve
        getsolve <- function() IM
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##  Function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        IM <- X$getsolve()
        if(!is.null(IM)) {
                message("getting cached data")
                return(IM)
        }
        data <- X$get()
        IM <- solve(data, ...)
        X$setsolve(IM)
        IM
}

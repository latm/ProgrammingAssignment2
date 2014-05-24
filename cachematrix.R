##Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# functions:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
#   makeCacheMatrix above. If the inverse has already been calculated 
#   (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#   from the cache.

## Write a short comment describing this function
# create a list containing a function to set the value of the matrix, get the value 
# of the matrix, set the value of inverse of the matrix and last get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inversa  <- NULL
    set  <- function(y) {
        x <<- y
        inversa <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) inversa  <<- inverse
    getinverse  <- function() inversa
    list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function returns the inverse of the matrix. 
# Case 1 - inverse has already been computed - If so, it gets the result and skips the
# computation. 
# Case 2 - inverse has already been computed - If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversa  <- x$getinverse()
    if (!is.null(inversa)){
        message("getting cached data")
        return(inversa)
    }
    data  <- x$get()
    inversa  <- solve(data, ...)
    x$setinverse(inversa)
    inversa    
}

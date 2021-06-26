## Title: Coursera - R Programming - Programming Assignment 2
## Author: Christian Thyssen
## Date: 2021-06-26
## Defines two functions:
## - makeCacheMatrix: Makes a data structure (a list) containing functions for
##                    getting and setting a matrix and its inverse matrix.
## - cacheSolve: Returs the inverse matrix of an invertible matrix wrapped into
##               a cache matrix. Stores the inverse matrix in the cache matrix
##               for future use.

## Returns a cache matrix for an invertible matrix, i. e., a list containing the
## following functions:
## - set(y): Sets the matrix to y and sets the inverse matrix to NULL.
## - get(): Gets the matrix.
## - setinverse(inverse): Sets the inverse matrix to inverse.
## - getinverse(): Gets the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns the inverse matrix of a cache matrix (see makeCacheMatrix).
## If the inverse matrix has been calculated before the cached result is
## returned.
## If the inverse matrix has not been calculated before it is calculated using
## solve and cached for future use.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

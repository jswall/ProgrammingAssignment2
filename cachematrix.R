# This file contains functions for calculating the inverse of a matrix 
# and caching that inverse for later reuse, thus eliminating the need 
# to recalculate.
# makeCacheMatrix creates an object that holds the matrix and the cached
# inverse and solveCache returns the inverse of a marix making use of
# the possibly cached value.


# Creates a matrix with cacheable inverse from a source matrix
#
# Arguments:
#  x: the source matrix
#
# Returns:
#  An object/list holding the original matrix with accessor functions 
#  set and get for setting and getting the matrix and setinverse and
#  getinverse for setting and getting the cached inverse value.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Calculate the inverse of a matrix with caching.
# 
# Arguments:
#  x: The cache matrix the inverse of which is to be calculated.
#  This must be an object created by the makeCacheMatrix function. 
#  The source matrix must be inversible, otherwise an error will occur.
#
# Returns:
#  The inverse matrix of the source
cacheSolve <- function(x, ...) {
    # check if the inverse is already in the cache
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    # cache miss, calculate and store result in cache 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## The functions in this file return the inverse of
## of a matrix and cache the result for subsequent
## calls.

## This function creates a wrapper of a matrix by
## a list containing following functions:
##
##   1. set - set the value of the matrix
##   2. get - get the value of the matrix
##   3. setSolved - set the inverse of the matrix
##   4. getSolved - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    
    get <- function() x

    setSolved <- function(solved) inversed <<- solved
    
    getSolved <- function() inversed
    
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## This function calculates the inverse of the matrix
## with the above function. It first checks to see
## if the inverse of the matrix has already been
## calculated. If so, it gets the inverse of matrix
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets
## the value in the cache.

cacheSolve <- function(x, ...) {
    solved <- x$getSolved()
    
    if (!is.null(solved)) {
        message("getting cached data")
        return(solved)
    }
    
    data <- x$get()
    solved <- solve(data)
    x$setSolved(solved)
    
    solved
}

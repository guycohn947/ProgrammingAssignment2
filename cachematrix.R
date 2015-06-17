## Put comments here that give an overall description of what your
## functions do

## Name: makeCacheMatrix()
## Purpose: create a matrix object that stores it matrix inverse
## Arguments: x - matrix of floats to be stored
## Returns: matrix object created

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Name: cacheSolve()
## Purpose: Find inverse of matrix using cached data or, if not possible, 
## by calculating it
## Arguments: x - matrix object created by makeCacheMatrix()
##            ... - additional arguments for solve()
## Returns: inverse of matrix in input matrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

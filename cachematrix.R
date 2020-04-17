## Cesar Benjumea
## Assignment 2

## makeCacheMatrix is a function that stores a matrix and returns
## a list containing the matrix along with other attributes to caching
## the inverse of a matrix function.
##
## Attributes of the list are:
## set - function that stores the matrix to be inverted
## get - function that retrieves the matrix to be inverted
## setsolve - function that stores the inverse of matrix of a matrix m
## getsolve - function that retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve calculates the inverse matrix of the special "matrix" created with 
## the makeCacheMatrix function. However, it first checks to see if the inverted
## matrix has already been calculated. If so, it gets the inverted matrix from 
## the cache and skips the computation. Otherwise, it calculates the inverted matrix
## the matrix and sets said value in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

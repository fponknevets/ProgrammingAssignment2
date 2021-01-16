## Steven Knopf 16th Jan 2021
## Coursera R Programming > Week 3 > Programming Assignment Lexical Scope
## coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping


## Functions to create and special matrix which can cache its inverse and can
## returns the cached inverse

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: computes the inverse of the special "matrix" returned
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i}

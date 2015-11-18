## The functions makeCacheMatrix and cacheSolve are used together to
## create a system that caches matrix inverse calculations.
## 
## makeCacheMatrix is a matrix like object that is able to save its
## own inverse internally.
## 
## cacheSolve is a function that operates on objects returned by
## makeCacheMatrix to either compute or retrieve a previously computed
## and cached value for the matrix's inverse.
## 

# makeCAcheMatrix
# 
# A function that returns a list object whose members are functions
# that operate on the saved internal state produced by the lexical
# scope (closure).
# 
# makeCacheMatrix takes an optional matrix argument and returns a
# pseudo "matrix" object that saves and retrieves internal state
# through the use of four functions. If the matrix argument is not
# supplied a default 1x1 matrix initialized to NA is used.
#
# The functions are:
# 
# set(y): Saves the value y internally as x
# 
# get(): Retrieves the saved internal value previously set by set(y)
#        or the result of matrix() if none was set
# 
# setinverse(inverse): Saves the supplied value inverse to an internal variable
# 
# getinverse(): Retrieves the saved value of inverse previously set by
#               setinverse(inverse) or NULL if none was set
# 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve
# 
# A function that computes the inverse of a "matrix" object previously created
# with makeCacheMatrix and makes use of caching to avoid recomputing the
# inverse if the matrix has not changed.
# 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if ( ! is.null(i) ) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
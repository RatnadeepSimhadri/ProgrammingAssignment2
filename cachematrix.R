# Matrix inversion is a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix and display the inverse from cache.

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inverseMat <- NULL
    set <- function(y) {
        x <<- y
        inverseMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMat <<- inverse
    getInverse <- function() inverseMat 
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been calculated. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setInverse function.

cacheSolve <- function(x, ...) {
        inverseMat <- x$getInverse()
    if(!is.null(inverseMat)) {
        message("getting Inverse matrix from Cache")
        return(inverseMat)
    }
    dataMatrix <- x$get()
    inverseMat<- solve(dataMatrix )
    x$setInverse(inverseMat)
    inverseMat
}

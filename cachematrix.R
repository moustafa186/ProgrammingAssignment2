## The below functions calculate and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse,
## which is really a list containing functions to:
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the 'makeCacheMatrix' function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrixdata <- x$get()
    inv <- solve(matrixdata, ...)
    x$setinverse(inv)
    inv
}

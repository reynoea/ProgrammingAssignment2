##Programming Assignment 2
##Assignment: Caching the Inverse of a Matrix
##First, I'll make a "matrix" that conatins functions to set the values, get the values, set the invesers, and the get inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##This next function will calculate the inverse of the "matrix" created with the makeMatrix function. First, it wiil
##check to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the compuation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

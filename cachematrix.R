## These two function are used to improve the efficiency of the computation of inverse matrix

## Below is a function to cache the inverse matrix of "x"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used to calculates the inverse matrix of the matrix created with the above function. 
## However, it first checks to see if the inversion has already been calculated.
## If so, it gets the inversion from the cache and skips the computation.
## Otherwise, it calculates the inversion of the data and sets the value of the inversion in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

## Functions that creates a special matrix construct that
## caches the computation of its inverse

## Creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Computes or retrieves a cache of 
## the inverse of a special matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    mtx <- x$get()
    m <- solve(mtx)
    x$setinverse(m)
    m
}

## The first function creates an R object that stores a matrix and its inverse.
## It consists of a set of four functions: set(), get(), setinverse(), and  
## getinverse(). It also includes two data objects: x and i (inverse of x).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function requires argument that is returned by makeCacheMatrix() 
## in order to retrieve the inverse matrix from the cached value that is stored 
## in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

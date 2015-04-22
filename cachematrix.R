## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## cacheSolve() function calculates the special "matrix" inverse if its not already cached

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Sets initial value to null
    inverse <- NULL
    ## define set function to initialize x matrix and clear cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## gets x matrix
    get <- function() x
    ## sets inverse cache
    setInverse <- function(inv) inverse <<- inv
    ## gets inverse from cache
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

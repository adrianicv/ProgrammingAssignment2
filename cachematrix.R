## These functions create a matrix and they allow to  
## compute the inverse matrix, and if the inverse is 
## already compute, get it from the cache

## Creates a matrix structure with getters and setters
## functions for the matrix and for it inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## set function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get function
    get <- function() x
    
    ## set inverse matrix function
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Gets the inverse of the matrix from the cache
## and if it isn't calculated yet it computes it
## and sets it in the structure

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Gets the inverse
    m <- x$getinv()
    
    ## If it is calculated it gets the inverse from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If it's not it calculates the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

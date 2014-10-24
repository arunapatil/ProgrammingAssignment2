## There are two functions in this file. The makeCacheMatrix function returns a list of functions that 
## get/set a matrix and get/set the inverse of a matrix. The cacheSolve function returns the inverse
## of the matrix, using a cached value if already computed.

## The makeCacheMatrix creates a special vector, which is a list of functions to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## This is the cached value for the inverse of the matrix
    inverse <- NULL
    
    ## This function sets the matrix and sets inverse to NULL
    set <- function(mat) {
        x <<- mat
        inverse <<- NULL
    }
    
    ## This function returns the matrix
    get <- function() {
        x
    }
    
    ## This functions caches the inverse of the matrix
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    ## This function returns the inverse of the matrix
    getinverse <- function() {
        inverse
    }
    
    ## Create a list for returning all 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks to see if the matrix inverse is already cached. If so, it returns the value in cache, else
## it computes the inverse of the matrix and sets it in the cache for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if inverse if already cached
    inv <- x$getinverse()
    if(!is.null(inv)) {  ##inverse is cached
        message("getting cached data")
        return(inv)
    }
    
    ## if inverse is not cahed, compute it
    mat <- x$get()
    inv <- solve(mat, ...)
    
    ## cache the inverse for future use
    x$setinverse(inv)
    
    ## return inverse
    inv
}

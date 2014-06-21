## Below are two functions that are used to create a special object that 
## stores a square invertible matrix and cache's its inverse

## The first function, makeCacheMatrix creates a list containing a function to
##  1- set the value of the matrix
##  2- get the value of the matrix
##  3- set the value of the inverse
##  4- get the value of the inverse


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


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the  
## cacheSolve function.

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i) ## Returning the cached matrix that is the inverse of 'x'
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i ## Returning a newly calculated matrix that is the inverse of 'x'
}

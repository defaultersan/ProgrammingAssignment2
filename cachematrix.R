## makeCacheMatric function is in turn exposing various functions to 
##      get/set the value of a matrix
##      get/set the inverse of a square matrix
## cacheSolve function computes the inverse of a matrix.
##      If the inverse matrix has already been calculated (and the matrix has not changed), 
##      then retrieves the inverse from the cache


## Creates and exposes following functions 
##      set function sets the value of the matrix
##      get function gets the value of the matrix
##      setinverse function sets the inverse of the square matrix
##      getinverse function gets the inverse of the square matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    ## Sets the cache with the inverted matrix
    setinverse <- function(solve) m <<- solve
    
    ## Get the inverse matrix
    getinverse <- function() m
    
    ## Expose all functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a square matrix that was passed in
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    ## Check if inverse has already been computed
    if(!is.null(m)) {
        ## if yes, then return the cached information
        message("getting cached data")
        return(m)
    }
    
    ## Inverse has not been processed yet, so go ahead with the processing
    data <- x$get()
    m <- solve(data, ...)
    
    ## Set the cache indicating inverse computed
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}

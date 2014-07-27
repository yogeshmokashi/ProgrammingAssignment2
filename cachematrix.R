## makeCacheMatrix - This function creates a special "matrix" object that can
##                   cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## variable to store inverse of matrix
    matinv <- NULL
    
    ## set function
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    
    ## get function
    get <- function() x
    
    ## set inverse of matrix function
    setinverse <- function(inv) matinv <<- inv
    
    ## get inverse of matrix function
    getinverse <- function() matinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already
##              been calculated (and the matrix has not changed), then 
##              this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## check whether matrix inverse has been calculated or not
    matinv <- x$getinverse()
    
    ## if inverse is calculated already (not null), return it
    if(!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    
    ## inverse is not calculated, calculate inverse of input matrix parameter
    ## use solve function on matrix
    data <- x$get()
    matinv <- solve(data)
    
    ## set inverse of matrix
    x$setinverse(matinv)
    
    ## return inverted matrix
    matinv    
}


## This pair of functions, makeCacheMatrix and cacheSolve, are used
## to find the inverse of a matrix and cache the result.  The next time 
## cacheSolve is used on the same matrix, the cached result is retrieved
## instead of performing the computation again.


## Creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Set the variables "x" and "i" in the parent environment to "y" 
        ## and "null," repsectively.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Pass the matrix "x" when called.
        get <- function() x
        
        ## Set the variable "i" in the parent environment to "inverse."
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        ## Return a makeCacheMatrix object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Computes the inverse of a matrix created with makeCacheMatrix, but
## returns the cached result if it exists.

cacheSolve <- function(x, ...) {
        
        ## If the inverse of "x" is cached, store in "i."
        i <- x$getinverse()
        
        ## If "i" contains a cached matrix, tell the user and return the data.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Compute the inverse of matrix "data" and store it in the variable
        ## "i."
        i <- solve(data, ...)
        
        ## Cache the inverse of "x."
        x$setinverse(i)
        
        ## Return the inverse of the matrix "x."
        i
}

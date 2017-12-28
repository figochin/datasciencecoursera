## This is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize an inverse matrix, "inv"
    inv <- NULL
    
    # Set the matrix, "x"
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse of matrix
    setInv <- function(solve) inv <<- solve
    
    # Get the inverse of matrix
    getInv <- function() inv
    
    # Return the list of functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Get the inverse matrix
    inv <- x$getInv()
    
    # Check if inverse is within the cached data. If yes, retrieve the inverse matrix.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix, "x"
    data <- x$get()
    
    # Calculate the inverse matrix
    inv <- solve(data, ...)
    
    # Set the inverse matrix
    x$setInv(inv)
    
    # Return the inverse of "x"
    inv
}
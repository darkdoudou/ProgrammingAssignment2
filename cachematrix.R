## The function makeCacheMatrix is a list of functions to be used by the second function cacheSolve
## The first function stocked the inverse of a matrix as cached data, in order to reuse it if already calculated.
## The second function use the cached data if it exist, or calculate the inverse of the matrix if not

## This first function create cached data in order to reuse it
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                # This part set the matrix and the inverse of the matrix
        x <<- y                         #
        inv <<- NULL                    # 
    }                                   #
    get <- function() x                 # 
    setInverse <- function(solve) inv <<- solve # The second function cacheSolve will use this part to stock
    getInverse <- function() inv                # the inverse of the matrix in the variable inv
    list(set = set,                             # Note that <<- allows us to not limit inv to the environment of the function 
         get = get,                             # but also outside of this function
         setInverse = setInverse,
         getInverse = getInverse)

}

## This function use the cached inverse matrix of x if it already exists, or calculate it if not
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()               # Use the previous function to get the inverse of the matrix x
    if(!is.null(inv)) {                 # Check if the inverse of x have been already calculated
        message("getting cached data")  # if it is the case, show the message that it's using cached data
        return(inv)                     # and return cached data
    }
    data <- x$get()
    inv <- solve(data, ...)             # if not, calculate the inverse of x
    x$setInverse(inv)                   # stock it as cached 
    inv                                 # and return the inverse of x
    
}

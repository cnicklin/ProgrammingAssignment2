## These two functions will allow you to input an invertible matrix and store the computation of the inverse so that you don't have to compute it again
## You can just call the cached values of the original matrix or its inverse
## You can also change those cached values manually without running the entire function again

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## set m within the makeCacheMatrix environment to NULL
        m <- NULL
        ## create a function called "set" that takes argument "y"
        ## this will allow you to change x within a makeCacheMatrix special "matrix" object without actually calling makeCacheMatrix again
        set <- function(y) {
                ## set x in the special "matrix" object to y
                x <<- y
                ## set m in the special "matrix" object to NULL
                m <<- NULL
        }
        ## create a function called "get" that has no arguments
        ## it will get the value of x from the special "matrix" object
        get <- function() x
        ## create a function called "setinverse" that takes argument "my_inverse"
        ## whatever you give to the my_inverse argument will be assigned to m within the special "matrix" object
        setinverse <- function(my_inverse) m <<- my_inverse
        ## create a function called "getinverse" that has no arguments
        ## it will get the value of m from the special "matrix" object
        getinverse <- function() m
        ## create a list of the four functions that were just defined
        ## assign names to them so that they can be called individually from the console
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## since the last action was creating that list, the list becomes the output
}


## computes the inverse of the special "matrix" returned by the makeCacheMatrix function
## if the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache
## if the inverse has not yet been calculated, cacheSolve will calculate it and it will get stored in the makeCacheMatrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## this function requires a makeCacheMatrix list as input
        ## call the getinverse function from the makeCacheMatrix list, which gets the value of m from within that special "matrix" object
        ## assign that value to m here in cacheSolve
        m <- x$getinverse()
        ## if the new value of m is *not* NULL, then...
        if(!is.null(m)) {
                ## print the following message
                message("getting cached data")
                ## and finish the cacheSolve function by returning m as the result
                return(m)
        }
        ## if m is NULL, the previous if loop would not have been executed so the program continues
        ## call the get function from the makeCacheMatrix list, which gets the value of x from within that special "matrix" object
        ## assign that value to the new variable "my_matrix"
        my_matrix <- x$get()
        ## get the inverse of the my_matrix object
        ## assign that value to m within the cacheSolve environment
        m <- solve(my_matrix, ...)
        ## call the setinverse function from the makeCacheMatrix list, which sets the value of m within that special "matrix" object to the value of the cacheSolve m that we just computed
        x$setinverse(m)
        ## return the cacheSolve m as the result
        m
}

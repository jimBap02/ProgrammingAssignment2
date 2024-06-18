
## makeCacheMatrix creates a list of named function closures which 
## represent a matrix that can cache its inverse:
##    Name      Function    Action
##    'set'     set(y)      sets the represented matrix to 'y'
##    'get'     get()       returns the represented matrix
##    'setinv'  setinv(inv) sets the inverse of the represented 
##                          matrix to 'inv'
##    'getinv'  getinv()    returns the inverse of the represented 
##                          matrix, computing (and locally saving it)
##                          as needed

makeCacheMatrix <- function(x = matrix()) {
    ##  'x' is a matrix which is ASSUMED to be invertible. 
    ##  NOTE: there is no check to insure that x is invertible, and 
    ##  there is no error recovery from a non-invertible x other than
    ##  the failure of solve() in cacheSolve
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of a cache matrix representation
## as constructed with makeCacheMatrix. If the cache matrix already
## has a locally stored inverse, it is returned; otherwise, the 
## inverse is computed, stored in the cache matrix using its setinv
## named function, and then returned.

cacheSolve <- function(x, ...) {
    ## 'x'  a cache matrix representation as constructed with 
    ##      makeCacheMatrix; x is ASSUMED to be invertible, and
    ##      there is no error recovery from a non-invertible x 
    ##      other than the failure of solve()
    ## '...' other parameters to be passed to solve in computing
    ##      the inverse.
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix,...)
    x$setinv(inv)
    inv
}

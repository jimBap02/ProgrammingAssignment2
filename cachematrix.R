
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

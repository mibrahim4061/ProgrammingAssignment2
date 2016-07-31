## This assignement is to cache the calculation result into an object for future use if it has been calculated once. 
## This is done to avoid the overhead of repeated calculations if the result is already known.
## If not present in cache, then the result is calculated.

## makeCacheMatrix -> Create a object that can cache the wanted result for future purposes.
## cacheSolve -> Tries the fetch the object from the cache. If it is a cache miss, then inverse is calculated anyway.

## This function creates a matrix object that cache(s) its inverse.

makeCacheMatrix <- function(x = matrix()) {

    q <- NULL
    
    ## set the matrix
    mset <- function(p) {
        y <<- p
        q <<- NULL
    }
    
    ## get the matrix
    mget <- function() y
    
    ## set the already inversed matrix for future use
    setinvmatrix <- function(inv=matrix()) q <<- inv
    
    ## get the inversed matrix for display
    getinvmatrix <- function() q
    
    list(mset = mset, mget = mget,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## This function tries the fetch the object from the cache. If it is a cache miss, then inverse is calculated anyway.

cacheSolve <- function(x, ...) {
    
    q <- x$getinvmatrix()
    
    ## Fetch the object from the cache if present.
    if(!is.null(q)) {
        message("getting cached inverse matrix")
        return(q)
    }
    
    ## Else inverse it here...
    invmatrix <- x$mget()
    z <- solve(invmatrix)
    x$setinvmatrix(z)
    
    ## Return a matrix that is the inverse of 'x'
    z
}

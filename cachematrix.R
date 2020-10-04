## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The pair of functions below cache the inverse of a matrix. It is
## assumed that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    set_inv <- function(inv1) {
        inv <<- inv1
    }
    
    get_inv <- function() {
        inv
    }
    
    list(
        set = set,
        get = get,
        set_inv = set_inv,
        get_inv = get_inv
    )

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    inv <- x$get_inv()
    
    if (!is.null(inv)) {
        message('Getting cached inverse matrix.')
        return(inv)
    }
    
    input_mat <- x$get()
    inv <- solve(input_mat, ...)
    x$set_inv(inv)
    
    inv
}

A = matrix(1:4, 2, 2)
# solve(A)

invA <- makeCacheMatrix(A)
cacheSolve(invA) # inverse is calculated.
cacheSolve(invA) # inverse recovered from cached data.

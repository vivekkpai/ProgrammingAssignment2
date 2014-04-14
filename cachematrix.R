## This file implements a caching matrix which can
## cache its own inverse. The file provides three
## functions:
##  (1) makeCacheMatrix(x = matrix()), which creates
##      a caching matrix
##  (2) cacheSolve(x, ...), which returns the inverse
##      of a caching matrix
##  (3) testCacheMatrix(), which tests the above two
##      functions on a pre-defined (hardcoded) matrix.
##      The function demonstrates how to call the functions
##      as well. To execute simply call:
##          testCacheMatrix()
##      with no parameters
##
##  Note that cacheSolve() assumes that the matrix is
##  invertible always.

## makeCacheMatrix(x = matrix())
##
## Creates a 'caching matrix' that can cache and
## return its inverse if already available. 
##
## Arguments:
## ---------
##  x: a regular R martix object. Default is a 1x1 matrix
##     with a NA value
##
## Value:
## -----
## The function returns a list with named elements
## each of which is a getter/setter function. These
## functions (which are closures) manipulate the
## environment to store the matrix as well as the
## inverse; and to retrieve it when required. 
##
## Implementation notes:
## --------------------
## In the code below, 'x' stores the matrix and inv
## stores the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    # The inverse of the matrix is initially NULL
    # and is part of the environment for the nested
    # functions (closures)
    inv <- NULL
    
    # Functions to get and set the matrix
    set <- function(mat) {
        # The <<- operator changes the value in the
        # environment. This is used to hold the state
        x <<- mat
        inv <<- NULL # Since the matrix has changed clear the cache
    }    
    get <- function() x    
    
    # Functions to get and set the inverse
    setInverse <- function(matinv) inv <<- matinv
    inverse <- function() inv
 
    # Return a list of named closures which can be used
    # to get/set the matrix and get/set the inverse of
    # the matrix.
    list(set = set,
         get = get,
         setInverse = setInverse,
         inverse = inverse)
}

## cacheSolve(x, ...)
##
## Returns the inverse of the caching matrix created by
## makeCacheMatrix(x). The function can be passed additional
## parameters for the solve() function. See ?solve() in console
##
## Arguments:
## ---------
##  x:      A caching matrix as created by makeCacheMatrix()
##  ...:    Other arguments that can be passed to the solve() function
##
## Value:
## -----
##  The inverse of the caching matrix. The inverse is always assumed to
##  exist. I.e. the matrix is always assumed to be invertible
##
## Implementation notes:
## --------------------
## The function checks whether an inverse was already computed
## earlier by retrieving a cached copy. If the cached copy exists
## it is returned. Else, the inverse is computed and cached for
## future usage
cacheSolve <- function(x, ...) {
    # Get the cached inverse if it exists
    inv <- x$inverse()
    
    if (is.null(inv)) {
        # If the inverse was not cached earlier comput it now
        inv <- solve(x$get(), ...)
        
        # And cache it for future usage
        x$setInverse(inv)
    }
    
    # Return the inverse
    inv
}

## testCacheMatrix()
##
## Test function that computes the inverse of the matrix:
##      0   0   1
##      2  -1   3
##      1   1   4
##
## Expected result:
##     -7/3     1/3     1/3
##     -5/3    -1/3     2/3
##        1       0       0
##
## Note that the values are printed in decimal point
## notation equivalent to the above result
testCacheMatrix <- function() {
    # Make an empty caching matrix
    m <- makeCacheMatrix()
    
    # Set the matrix as defined above    
    m$set(matrix(c(0, 2, 1, 0, -1, 1, 1, 3, 4), 3, 3)) 
    
    # Print the input matrix by doing a get on the caching matrix
    cat("Input Matrix:\n")
    print(m$get())
    
    # Compute the inverse by calling cacheSolve(x, ...)
    inv <- cacheSolve(m)
    
    # Print the result
    cat("\nInverse of matrix:\n")
    print(cacheSolve(m))
}

## Functions for matrices caching
## By taking advantage of R's lexical scoping rules matrices and its inversion 
## that may require long or repeated computations are stored in cached variables,  
## allowing for quick retrieval and computing only when changes occur.
## IMPORTANT: the functions assume the matrix passed as parameter is invertible.


## makeCacheMatrix - Function ##
## supply an invertible matrix as x parameter
## the function stores the matrix and its inversion in its environment
## and provides a list of four functions to set and retreive the matrix and its
## inversion. Use x$function() to access each of the functions.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() as.matrix(x)
        set_inverse <- function(inverse = matrix()) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve - Function ##
## supply a list created with the makeCacheMatrix function. cacheSolve will 
## return the inverted matrix ether by caluclating it (if it doesn't exist) or 
## by retreiving it form cache if it has been already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_invers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}

## Functions for matrices caching
## By taking advantage of R's lexical scoping rules matrices that may require
## long or repeated computations are stored in a cached variable(list), allowing 
## for quick retrieval and computing only when changes occur.
## IMPORTANT: the functions assume the matrix passed as parameter is invertible.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() as.matrix(x)
        set_inverse <- function(inverse = matrix()) m <<- as.matrix(inverse)
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

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

## Functions for matrices caching
## By taking advantage of R's lexical scoping rules matrices and its inversion 
## that may require long or repeated computations are stored in cached variables,  
## allowing for quick retrieval and computing only when changes occur.
## IMPORTANT: the functions assume the matrix passed as parameter is invertible.


## makeCacheMatrix - Function ##
## supply an invertible matrix as xm parameter
## the function stores the matrix and its inversion in its environment
## and provides a list of four functions (set(), get(), get_inverse(), 
## set_inverse()) to set and retreive the matrix and its inversion. 
## Use xm$function_name() to access each of the functions.

makeCacheMatrix <- function(xm = matrix()) {
        
        xim <- NULL #inverted matrix (xim)
        ## set (xm)
        set <- function(y) {
                xm <<- y #matrix (xm)
                xim <<- NULL
        }
        ## get (xm)
        get <- function() {
                as.matrix(xm)
        }
        ## set inverse (xim)
        set_inverse <- function(inverse = matrix()) {
                xim <<- inverse
        }
        ## get inverse (xim)
        get_inverse <- function() {
                xim
        }
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve - Function ##
## supply a list created with the makeCacheMatrix function. cacheSolve will 
## return the inverted matrix ether by caluclating it (if it doesn't exist) or 
## by retreiving it form cache if it has been already calculated.

cacheSolve <- function(xm, ...) {

        xim <- xm$get_invers() # inverted matrix (xim)
        if(!is.null(xim)) {
                message("getting cached data")
                return(xim)
        }
        data <- xm$get() #matrix (xm)
        xim <- solve(data, ...)
        ## Return a matrix that is the inverse of 'm'
        xm$set_inverse(xim)
        xim
}

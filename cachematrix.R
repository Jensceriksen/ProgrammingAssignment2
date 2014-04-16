## This script contains two functions:
## 'makeCacheMatrix' - A function that creates a special matrix object that can cache its inverse matrix
## 'cacheSolve'      - A function that returns the cached value. If the value has not been computed yet, 
##                     it computes the inverse and stores it in the cache, 
## 
## The functions were made for assignment 2, in the Coursera 'R Programming' course.
## The code was written by using and altering the already existing vector example from the assignment information.

## 'makeCacheMatrix' creates a matrix object that can cache its inverse matrix.
makeCacheMatrix <- function(x = matrix) {
       
        # First, any stored inverse value (s) is set to NULL
        s <- NULL
        
        # 'set' function that can be used to set the input matrix 
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # 'get' function that is used to get the matrix
        get <- function() x
        
        # 'set_inverse' is used to cache the inverse value
        set_inverse <- function(solve) s <<- solve
        
        # 'get_inverse' is used to get the cached inverse value
        get_inverse <- function() s
        
        # A list containing the four functions described
        # above, 'set', 'get', 'set_inverse', 'get_inverse'
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}



## 'cacheSolve' calculates the inverse of a matrix. If the inverse has already
## been calculated for this matrix, cacheSolve should return the inverse from
## the cached inverse in 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        
        # Getting the cached inverse value
        s <- x$get_inverse()
        
        # If the cached value is not NULL, the matrix is the same
        # and the cached value is returned
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # If the matrix is not the same, the new matrix is loaded
        data <- x$get()
        
        # The new matrix is inversed by use of the 'solve' function
        s <- solve(data, ...)
        
        # The inverse matrix is cached in 'set_inverse'
        x$set_inverse(s)
        
        # The inverse matrix is returned
        s
}

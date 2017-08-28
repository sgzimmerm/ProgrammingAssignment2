## This program, cachematrix.R, caches the inverse of a matrix
## so that for a given matrix, the inverse is computed only 
## once, even if the result is needed multiple times.

## The function, makeCacheMatrix, takes as input
## an invertible matrix and returns a special object
## which is a list of functions that sets the value
## of the matrix, gets the value of the matrix,
## sets the value of the inverse of the matrix, and
## gets the value of the inverse of the matrix.
## An error will occur if the input matrix
## is not invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y)      {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) inv <<- solve
        
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The function, cacheSolve, takes as input the list
## of functions returned by makeCacheMatrix. It
## checks to see if the inverse of the special
## matrix stored by makeCacheMatrix has already 
## been calculated. If not, it gets the original
## matrix (data), calculates the inverse, and sets
## the value of the inverse in the cache using the
## setinverse function. If the inverse has been
## calculated, it skips the calculation and gets
## the inverse of the matrix from the cache.
## This function returns a matrix that is 
## the inverse of of the input matrix.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        if(!is.null(inv))  {
                
                message("getting cached data")
                
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
        
}

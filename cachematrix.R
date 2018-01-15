## Below are two functions, makeCacheMatrix & cacheSolve.
## Together, these functions will cache the inverse of a matrix.
## Because determining the inverse of a matrix is computationally intensive,
## storing the inverse, then recalling it (instead of continually computing it)
## would significantly decrease run time and generate the output much faster.
## For instance, determining the inverse of a 500x500 matrix may require repetitive
## calculations of many of the same numbers. If calculations were stored,
## the next time that same calculation needs to be ran, 
## it can instead retrieve the value from the cache, saving calculation time.


## makeCacheMatrix() returns a list. This list contains a function that will:
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse matrix value
## 4. get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize invrs object. Any invrs values will be nullified.
        invrs <- NULL
        
        # Store the input matrix
        set <- function(y) {
                
                # Assign the input matrix to this current environment
                x <<- y
                
                # Nullify any invrs value
                invrs <<- NULL
        }
        
        # Retrieve the matrix
        get <- function() x
        
        # Store the inverse of the matrix
        set_invrs <- function(solve_outpt) invrs <<- solve_outpt
        
        # Retrieve the inverse of the matrix
        get_invrs <- function() invrs
        
        # Gives the name 'set' to set(), 'get' to get(), etc.
        # Makes it so that the function can be accessed using $ operator
        # Return as a list
        list(set = set, get = get, set_invrs = set_invrs, get_invrs = get_invrs)
}


## cacheSolve() returns the inverse of the matrix.
## First, it will check if the inverse has been calculated. If TRUE, it will
## retrieve the value from the cache and skip the calculation. If FALSE, it will
## calculate the inverse and store that value in the cache.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
        # Checks if the inverse has been calculated.
        # The inverse value can be found in 'get_invrs' from makeCacheMatrix().
        invrs <- x$get_invrs()
        
        # If TRUE
        if(!is.null(invrs)) {
                
                # Print message and 
                # Return the inverse value that's been retrieved
                message("getting cached data")
                return(invrs)
        }
        
        # If FALSE
        # Calculate the inverse of the matrix
        data <- x$get()
        invrs <- solve(data, ...)
        
        # Store the inverse value
        x$set_invrs(invrs)
        
        # Return the inverse value
        invrs
}

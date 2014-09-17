## Functions implemented to precache possibly heavy calculations
## in order to save time when running a program

## Prepares the list that will take care of caching the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
}


## The function returns the inverse matrix of a given one
## either calculates it or returnes the previously cached inverse.
## It is meant to be used together with makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

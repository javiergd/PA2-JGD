## Functions implemented to precache possibly heavy calculations
## in order to save time when running a program

#######################################################
## Function: makeCacheMatrix
## Prepares the list that will take care of caching the 
## inverse of the matrix
#######################################################

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL #here we will store the inverse

        ##initialize the values of the return list
        ##that will compute the inverse matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        ##create the list 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
}

##################################################################
## Function: cacheSolve
## The function returns the inverse matrix of a given one
## either calculates it or returnes the previously cached inverse.
## It is meant to be used together with makeCacheMatrix()
##################################################################

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        
        ## if matrix is cached, return it
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## Store data and compute the inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

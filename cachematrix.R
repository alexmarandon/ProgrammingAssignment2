## These functions compute the inverse of a square matrix M 
## (as defined by the relationship M*inv(M)=1) 
## and cache the result if it already been computed

## the makeCacheMatrix creates a list of 4 functions around a Matrix:
## get() - Recalls the value of stored Matrix
## set(M) - Stores a new matrix M
## getinverse() - Recalls the value of Matrix inverse stored
## setinverse(M) - Stores the value of Matrix inverse
##
## Note: setinverse does actually not compute matrix inverse 
## this would be done by cachesolve
## so any value can be stored in with this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##set function
        set <- function(y) {
                x <<- y
                ##when new value set, reinitiate inverse
                inv <<- NULL
        }
        ##get function
        get <- function() x
        ##setinverse function
        setinverse <- function(inverse) inv <<- inverse
        ##getinverse function
        getinverse <- function() inv
        ##return list of 4 functions
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve computes the inverse of a matrix using the solve function
## it checks if previously computed  with getinverse()
## if yes, recalls matrix inverse along with message "getting cached data"
## if no, computes inverse, stores it with setinverse() and returns it.

cacheSolve <- function(x, ...) {
        ## initialize check if inverse already computed
        inv <- x$getinverse()
        ## if inverse previously computed, no need to compute 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##if not, let's recall matrix for inverse computation
        data <- x$get()
        ##inverse actually computed here:
        inv <- solve(data, ...)
        ##inverse stored with setinverse
        x$setinverse(inv)
        ##returns inverse
        inv
}

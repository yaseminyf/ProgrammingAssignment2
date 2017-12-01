## Function makeCacheMatrix: 
##      Creates a special "matrix" object that can cache its inverse
## Function cacheSolve: 
##      Computes the inverse of the special "matrix" returned by the above function.
##      If the inverse is already computed, it returns the cached inverse
## functions do

## Initialize the inverse matrix and define the set, get, setinverse and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse matrix to NULL
        
        m <- NULL
        
        ## Define the set function which sets the inverse to NULL and
        ## sets 'x' to some new matrix 'y'
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Define the get function that returns the cached 'x'
        
        get <- function() x
        
        ## Define the setinverse function that sets the inverse matrix 'm' 
        
        setinverse <- function(solve) m <<- solve
        
        ## Define getinverse function which returns the inverse matrix 'm'
        
        getinverse <- function() m
        
        ## Form and return the list of the above defined functions
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}

## Return a matrix that is the inverse of cached matrix 'x'  

cacheSolve <- function(x, ...) {
        
        ## Attempt to get the cached inverse of 'x'
        
        m <- x$getinverse()
        
        ## If there is a cached inverse, return that matrix
        
        if(!is.null(m)) {     
                message("getting cached data")
                return(m)
        }
        
        ## If there is no cached inverse, get the cached 'x',
        ## compute its inverse, cache the inverse matrix and
        ## return it to the caller
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

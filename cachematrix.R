## The two functions calculate the inverse of a matrix and store the result 
## in the cache in order to use the stored result and not recalculate the inverse
## every time this result is needed. 


## makeCacheMatrix creates a list of 4 functions to store or set informations
## that will be used by the cacheSolve function such as the initial matrix
## and the result of the matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the value of m
        m <- NULL  
        
        ## set stores the matrix inputed in the makeCacheMatrix environment. 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get can be called to show the matrix inputed 
        get <- function() x
        
        ## setinverse stores the inverse of the matrix in the 
        ## makeCacheMatrix environment.         
        setinverse <- function(inverse) m <<- inverse
        
        ## getinverse return the result of the inversion of the matrix.
        getinverse <- function() m
        
        ## The four matrices are stored in a list that can be called with
        ## the $ operator. 
        list(set = set, get = get, setinverse = setinverse, 
        getinverse = getinverse)
}


## cacheSolve determines if the inverse of the matrix x was already calculated. 
## If the inverse was already determined, the stored results is returned. 
## If the inverse was not already calculated, the inverse is calculated, 
## stored in the makeCacheMatrix environment and returned. 

cacheSolve <- function(x, ...) {
        
        ## Assign to m the result of the inverse of X store in the 
        ## MakeCacheMatrix environment
        m <- x$getinverse()
        
        ## Determine if the inverse was already calculated. If TRUE,
        ## the result is returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If m is null, x (store in cache) is assign to data 
        data <- x$get()
        
        ## The inverse of x is assign to m 
        m <- solve(data, ...)
        
        ## m is stored in the cache. 
        x$setinverse(m)
        
        ## m is returned
        m
}

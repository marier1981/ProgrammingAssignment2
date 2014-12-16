## Save the inverse of a matrix, such that it may be reused without reevaluating it

## Create the structure of the new object, which is able to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initially, the inverse is empty
        m <- NULL
        
        ## If we wish to reset the matrix to a new one, then the inverse is deleted
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Retrieve the stored matrix
        get <- function() x
        
        ## Assign the inverse of the matrix to 'm'
        setInverse <- function(inverse) m <<- inverse
        
        ## Retrieve the value of 'm', which when set, is the inverse
        getInverse <- function() m
        
        ## Make the object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return the inverse; if it has not been stored, then calculate it, otherwise retrieve it from cache
cacheSolve <- function(x, ...) {
        ## Retrieve what is stored as the inverse
        m <- x$getInverse()
        
        ## If the inverse exists (is not null) then return and exit
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise read in the matrix
        InverseMatrix <- x$get()
        
        ## Solve to obtain the inverse
        m <- solve(InverseMatrix, ...)
        
        ## Remember to store the inverse in the cache
        x$setInverse(m)
        
        ## Return the inverse
        m
}

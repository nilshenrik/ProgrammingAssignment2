## These two functions will together return the inverse of a matrix and cache it. 

## This first function creates a list which is used as an input to the second function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) Inv <<- solve
        getsolve <- function() Inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function will return the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, it gets the inverse from the cache.    

cacheSolve <- function(x, ...) {
        
        Inv <- x$getsolve()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setsolve(Inv)
        Inv
        
}

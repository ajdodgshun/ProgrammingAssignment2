## The overall functions ensure that the solve computation does not need to be repeated and can be
## cached in memory to be called up during later functions.

## Using exactly the same structure as the cachemean function given as an example,
## the makeCacheMatrix function stores an list of length 4 containing the functions set, get, 
## setinverse and getinverse.  This function requires minimal computation and is just a setup
## for the next phase.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve first checks the list created in the function above.  If there is already a value
## in this list for "getinverse" meaning the function has already been run earlier then it returns
## a message "getting cached data" and then returns that cached data.  If "getinverse" is null
## meaning the computation has not already been done then the function goes ahead and runs the
## solve function using the "setinverse" function within the list above.  It then returns the
## inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'

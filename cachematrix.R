## Creates a list which contains the matrix as well
## as any previously calculated results about it

## This function is used to create the Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## This function is used to solve the matrix or
## retrieve the cached solution if it was already
## calculated

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}

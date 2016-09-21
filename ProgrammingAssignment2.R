## Cache the matrix for inversion 
## This function creates a matrix which can store a corresponding inverse
## Used to cache potentially time-consumed computations

# this makecacheMatrix will create a special matix which can cache the inverse

makecacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <-function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# this cacheSolve will return the inverse of the matrix
# the function will first check to see if the inverse has been computed already
# if the inverse has been computed and the matrix has not been edited, 
# then the cacheSolve function skip the computation
# if the inverse has not been computed then the cache will retrieve the inverse

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

# r-program-assignment2
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        # x: output of makeCacheMatrix()
        # return : inverse of the original matrix input to makeCacheMatrix()
        inv <- x$getinv()
        # if the inverse has already been calculated
        if(!is.null(inv)) {
                #get it from the cache and skip the computation
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculates the inverse
        data <- x$get()
        inv <- solve(data, ...)
        #sets the value of the inverse in the cache via the setinv
        x$setinv(inv)
        return(inv)
}

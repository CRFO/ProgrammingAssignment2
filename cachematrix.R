#Cristiane Foust - Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        set <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inversion <<- solve
        getsolve <- function() inversion
        list (set = set,
              get = get,
              setsolve = setsolve,
              getsolve = getsolve)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inversion <- x$getsolve()
        if(!is.null(inversion)) {
                message("getting cached data")
                return(inversion)
        }
        data <- x$get()
        inversion <- solve(data, ...)
        x$setsolve(inversion)
        inversion
} 
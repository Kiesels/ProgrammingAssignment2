## a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                  ## set function
                x <<- y
                m <<- NULL
        }
        get <- function() x                   ## get function
        setinv <- function(solve) m <<- solve ## setinv function
        getinv <- function() m                ## getinv function
        list(set = set, get = get,            ## makeCacheMatrix output
             setinv = setinv,
             getinv = getinv)        
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {                     ## data in the cache
                message("getting cached data")
                return(m)
        }
        message("else: not in the cache, will calculate")
        data <- x$get()                       ## data not in the cache
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

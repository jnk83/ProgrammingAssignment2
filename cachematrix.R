## makeCacheMatrix creates a function that calculates the inverse of a Matrix.  Values can then be manipulated
## and cached thereafter directly from the object, instead of being forced to call makeCacheMatrix again.
## cacheSolve creates a function that checks to see if the inverse value of a Matrix is already cached in
## memory.  If so, the value is returned.  If not, cacheSolve calculates the inverse and caches the values.

## makeCacheMatrix creates a space in memory initially for a null matrix 'm' for later inversion.
## 'set' allows for later re-setting the matrix values through makeCacheMatrix directly. 'get' returns the 
## matrix values provided by the user. 'sinvert' sets the value of the inverse matrix and caches it. 
## 'ginvert' fetches the cached inverse matrix.  'list' creates a list and attaches the proper values to allow
## for use of the $ operator.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        sinvert <- function(solve) m <<- solve
        ginvert <- function() m
        list(set = set, get = get, sinvert = sinvert, ginvert = ginvert)
}


## cacheSolve first checks to see if a cached version of m exists by attempting to get 
## the values of the inverse matrix from makeCacheMatrix above.  The function checks to see
## if the value is null.  If not null, cacheSolve returns the stored values.  If no cached values exist,
## cacheSolve solves for the inverse, caches the values, and displays them to the user.


cacheSolve <- function(x, ...) {
        m <- x$ginvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$sinvert(m)
        m
}
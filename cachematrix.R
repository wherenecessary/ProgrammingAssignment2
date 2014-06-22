## Creates an invertible matrix, calculates its inverse and stores it in cache

## Creates an invertible matrix object and caches its inverse (calculated in cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Computes the inverse of the invertible matrix returned by makeCacheMatrix 
## or retrieves the inverse from cache if it has already been calculated

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$set_inverse(m)
        m
}

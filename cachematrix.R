# Matrix inversion is usually a time consuming computation. There may be benefits to to caching the inverse of a matrix
# rather than compute it repeatedly. The following functions are used to cache the inverse of a matrix.

## This function creates a special matrix object so that its inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of makeCacheMatrix above. It checks if the inverse has been been calculated before, if not  
## it then retrieves the inverse from the cache. This function assumes that the matrix is always invertible.         

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

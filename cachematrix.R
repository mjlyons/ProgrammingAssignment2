## cachematrix.R
##
## Adds caching support to matrix inversion. Calls to invert a matrix (after
## the first inversion of the matrix) return the cached result.
##

## Returns a an object which contains a matrix, and which can cache
## the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(matrixInverse) {
        mi <<- matrixInverse
    }
    getMatrixInverse <- function() mi
    list(set = set,
         get = get,
         setMatrixInverse = setMatrixInverse, 
         getMatrixInverse = getMatrixInverse)
}


## Finds the inverse of a matrix. If the inverse was already computed, the cached
## result is returned. Otherwise, the inverse is computed and cached.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getMatrixInverse()
    if(!is.null(mi)) {
        message("getting cached matrix inverse data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setMatrixInverse(mi)
    mi
}

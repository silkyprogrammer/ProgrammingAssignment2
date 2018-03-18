
## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
    inv <- NULL
    set <- function( matrix ) {
            matx <<- matrix
            inv <<- NULL
    }
    get <- function() {
    	## Return the matrix
    	matx
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Method to inverse "X" - which is returned by the above method.

cacheSolve <- function(x, ...) {
    matx <- x$getInverse()
    if( !is.null(matx) ) {
            message("getting cached data")
            return(matx)
    }
    data <- x$get()
    matx <- solve(data) %*% data
    x$setInverse(matx)
    matx
}
## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # var and functions
        matrizinversa <- NULL
        set <- function( y ){
                x <<- y
                matrizinversa <<- NULL
                }
                get <- function() ( x )
        setInversa <- function( calculoinversa ) ( matrizinversa <<- calculoinversa )
        getInversa <- function() ( matrizinversa )
        list( set = set, get = get, setInversa = setInversa, getInversa = getInversa )
        
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solinversa <- x$getInversa()
        if (!is.null(solinversa)) {
                 message( "getting cached data" )
                 return( solinversa )
                 }
        data <- x$get()
        solinversa <- solve(data, ...)
        x$setInversa(solinversa)
        solinversa
}

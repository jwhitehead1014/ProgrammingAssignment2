## These two functions, makeCacheMatrix and cacheSolve will allow for the
## caching of the inverse of a matrix, rather than having to compute repeatedly

## First, reate a special "matrix" that allows me to cache the inversion the matrix
## Set the matrix
## Get the matrix
## Set the inversion of the matrix
## Get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## This function actually calculates the inversion of the special "matrix"
## created in the function above, after first checking to see if that
## inversion has already been calculated.  If so, it gets it from the cache; if not
## it does the computation and sets the value of the inversion in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinverse(inv)
        inv
}
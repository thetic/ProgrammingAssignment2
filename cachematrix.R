## Wraps a matrix so that its inverse can be cached
## for constant time retrieval

## Creates a special matrix which can cache its inverse.
## A square, invertible matrix is assumed.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL  # initialize without inverse
    
    # to change to stored matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # to retrieve the stored matrix
    get <- function() {
        x
    }
    
    # to save the inverse
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    # to retrieve the inverse
    getinverse <- function() {
        inv
    }
    
    list(
        set = set,
        get = get,
        getinverse = getinverse,
        setinverse = setinverse
    )
}

## Solves and caches a matrix's inverse on first call
## then returns the cached inverse on following calls
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if(!is.null(inv)) {  # if the inverse was already calculated
        message("getting cached data") # get it
        return(inv)  # we're done!
    }
    # otherwise
    data <- x$get()  # get the matrix
    inv <- solve(data)  # invert it
    x$setinverse(inv)  # save it
    inv  # return it
  
}

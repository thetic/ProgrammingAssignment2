## Wraps a matrix so that its inverse can be cached
## for constant time retrieval

## Creates a special matrix which can cache its inverse.
## A square, invertible matrix is assumed.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
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
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  
}

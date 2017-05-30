## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(v) {
        m <<- v
        inverse <<- NULL
    }
    get <- function() m
    setinverse <- function(value) inverse <<- value
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    calculated <- solve(data, ...)
    m$setinverse(calculated)
    calculated
}

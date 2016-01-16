## Put comments here that give an overall description of what your functions do
# The functions makeCacheMatrix and cacheSolve will cache and compute the inverse of a matrix.

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set the value of the vector
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the vector
    get <- function() x
    # set the value of the mean
    setinverse <- function(solve) m <<- solve
    # get the value of the mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}



# Compute inverse of matrix the normal way  
a <- matrix(rnorm(16), 4, 4)
a
solve(a)

# using cache functions
A <- makeCacheMatrix(a)
cacheSolve(A)





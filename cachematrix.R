## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The file contains a pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    .set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    .get <- function() x
    .setinverse <- function(inverse) inv <<- inverse
    .getinverse <- function() inv
    list(set = .set, get = .get,
         setinverse = .setinverse,
         getinverse = .getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## x should be a square invertible matrix!
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


#tests
m1 <- makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2))
m1inv <- cacheSolve(m1) #no chache
m1inv <- cacheSolve(m1) #cached
m1$get() %*% m1inv
set.seed(12345)
m1$set(matrix (rnorm(25), nrow = 5, ncol = 5))
m1inv <- cacheSolve(m1) #no chache
m1inv <- cacheSolve(m1) #cached
round(m1$get() %*% m1inv, digits = 9)


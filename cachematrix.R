## This functions can cache inverse of Matrix
## Below function 1 makeCacheMatrix will be used to set and get value of matrix and its Inverse


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


## Below function 2 computes inverse of Matrix using funtion Solve
## if the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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

## End of Program <RAJNISH KUMAR SINGH: SUNDAY FEB, 21, 2016>

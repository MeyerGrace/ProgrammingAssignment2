## Grace Meyer 31/05/2016
##Matrix inversion is usually a costly computation and there may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly.

## This function caches the matrix that is passed to it and creates an inverse variable which is set to NULL
## It returns a list of 4 functions which can be used to cache and access the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse){ Inv <<- inverse} 
  getinverse <- function() {Inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function tries to access the inverse cached variable, if it is not NULL then it gets the matrix, calculates and caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$setinverse(Inv)
  Inv
}



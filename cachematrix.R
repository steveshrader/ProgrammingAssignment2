## Matrix inversion can be a computationally intensive process.  To avoid unneeded computation
## this code will cache unchanged inversions and return the correct information directly.
## If the original matrix has changed the inversion will be done, cached and returned


## makeCacheMatrix will wrap a matrix with the ability to return its inversion from cache if it has already been computed

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse_m <<- inverse_matrix
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will determine if the inversion is in cache or needs computed

cacheSolve <- function(x, ...) {
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  data <- x$get()
  inverse_m <- solve(data, ...)
  x$setinverse(inverse_m)
  inverse_m
}

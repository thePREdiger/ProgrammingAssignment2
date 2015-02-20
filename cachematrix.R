## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
			x <<- y
			m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
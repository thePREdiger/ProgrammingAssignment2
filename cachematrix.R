## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # store the cached inverse matrix
  m <- NULL
  set <- function(y) {
	  x <<- y
		m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # inversing
  matrix <- x$get()
  m <- solve(matrix, ...)
  
  # caching
  x$setmatrix(m)
  
  m
}
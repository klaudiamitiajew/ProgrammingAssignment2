## This set of functions creates a special "matrix" object that can cache its inverse.
## This is useful to avoid redundant computations when inverting the same matrix multiple times.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache for the inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cache when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse in cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the cached inverse instead of recomputing it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  
  # If inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse using solve()
  
  # Store the inverse in cache
  x$setInverse(inv)
  
  # Return the computed inverse
  return(inv)
}

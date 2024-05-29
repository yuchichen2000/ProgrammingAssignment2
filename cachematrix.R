## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for the inverse matrix as NULL
  
  set <- function(y) {
    x <<- y  # Set the matrix to a new value
    inv <<- NULL  # Reset the inverse cache because the matrix has changed
  }
  
  get <- function() x  # Get the current matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse matrix
  
  getInverse <- function() inv  # Get the cached inverse matrix
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  # Return a list of the above functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {  # If cached, return the cached inverse
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()  # Get the current matrix
  inv <- solve(mat, ...)  # Compute the inverse using solve function
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the computed inverse
}

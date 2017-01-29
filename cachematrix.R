## Put comments here that give an overall description of what your
## functions do

## Create a new matrix with cached inversion. 

makeCacheMatrix <- function(x = matrix()) {
  
  # New matrix, so set inverse to NULL. 
  inv <- NULL
  
  # Setter for the matrix x
  set <- function(y) {
    x <<- y
    # Matrix changed, so the inverse is no longer valid. Reset it to NULL
    inv <<- NULL
  }
  
  # Getter for the matrix x
  get <- function() x
  
  # Setter for the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Getter for the inverse matrix
  getinverse <- function() inv
  
  # return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # retrieve the current inverse value
  inv <- x$getinverse()
  
  # If this value is not NULL, it is already calculated. 
  # Return the already calculated value. 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If we reach this point, the inverse is not calculated yet. Calculate it now. 
  data <- x$get()
  inv <- solve(data, ...)
  
  # Store the inverse. 
  x$setinverse(inv)
  
  # Return the inverse. 
  inv
}

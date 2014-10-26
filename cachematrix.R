makeCacheMatrix <- function(x = matrix()) {
	## This function creates a special matrix object that can cache its inverse
  m <- NULL
  ## Method to set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## Method to get matrix
  setinverse <- function(solve) m <<- solve
  ## Method to set inverse of matrix
  getinverse <- function() m
  ## Method to get inverse of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
       ## Returns list of methods
}
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## Returns the inverse of 'x' if it has already been calculated rather than recalculating
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  ## Caluculates inverse of 'x' if necessary
  m
  ## Returns inverse of 'x'
}


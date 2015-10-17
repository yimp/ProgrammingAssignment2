## both functions are used in conjunction with the other to help
## reduce computation time required when the matrix inverse is computed
## if the same matrix is being used then it will cache the inverse,
## otherwise it will recompute the inverse

# makeCacheMatrix creates a list containing the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- as.null(matrix()) #make this a null matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve #this should compute the inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve checks if the inverse has been cached, otherwise recalculates it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # computes the mean if it hasnt been cached
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

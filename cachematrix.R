## The following two functions are used to cache the inverse of a matrix and
## retrieve the cached value instead of recalculating the inverse multiple times.

## This function creates a list of 4 functions which set the value of the matrix,
## get the value of the matrix, set the value of the inverse of the matrix, and get
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function first checks to see if the inverse of the matrix has already been
## calculated and returns it if it has. If not, it calculates the inverse, sets its
## value in the cache, and returns the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}
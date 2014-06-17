## This functions compute and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) i <<- inversematrix
  getinversematrix <- function() i
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The following function calculates the inverse of a matrix created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse and sets the
## result in the cache via setmatrixinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinversematrix(i)
  i
}

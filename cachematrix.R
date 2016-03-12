## JCarroll Assignment 2 R Programming Coursera course
## This program will cache the inverse of a matrix and solve using the cached inverse

## makeCacheMatrix function takes a matrix and caches the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Create a matrix that is the inverse of 'x'
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrieves the cached inverse matrix created in the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


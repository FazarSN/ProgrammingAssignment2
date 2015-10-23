## Put comments here that give an overall description of what your
## functions do
## this function will make any numeric input x as a list that containing matrix and inverse (null for starter)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  inverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, inverse = inverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function will cache the inverse of x if x is invertible (if not it will recall an error)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$inverse(m)
  m
}

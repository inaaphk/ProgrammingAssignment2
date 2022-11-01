## Caching the inverse of a Matrix

## This function will create a special 'matrix' object that
## can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}

## The next function computes the inverse of the
## special 'matrix' returned by 'makeCacheMatrix'.
## If the inverse was calculated previously, then
## 'cacheSolve' will retrieve the inverse of the
## cache.

cacheSolve <- function (x,...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %% data
  x$setInverse(m)
  m
}
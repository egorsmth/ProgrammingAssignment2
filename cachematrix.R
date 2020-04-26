## Creation of matrix and caching its reverse

## Creates special matrix that caches reverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setreverse <- function(r) m <<- r
  getreverse <- function() m
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
  
}


## Return a matrix that is the inverse of 'x' and cache result if matrix not changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getreverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setreverse(m)
  m
}

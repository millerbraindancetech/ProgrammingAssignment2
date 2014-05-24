## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      matinverse <- NULL
      set <- function(y) {
            x <<- y
            matinverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) matinverse <<- solve
      getinverse <- function() matinverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
}


## This function returns the inverse of the the matrix either from cache (if already calculated)
## or calculates it 

cacheSolve <- function(x, ...) {
      matinverse <- x$getinverse()
      if(!is.null(matinverse)) {
            message("getting cached data")
            return(matinverse)
      }
      data <- x$get()
      matinverse <- solve(data, ...)
      x$setinverse(matinverse)
      matinverse
}

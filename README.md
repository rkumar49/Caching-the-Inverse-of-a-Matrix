## These functions are to explore lexical scoping as part 
## ofCoursera course in R Programming with JHU 

## This function, makeCacheMatrix() contains the following functions
##
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse
##
## it returns a list of above functions
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function, cacheSove() calculate the inverse of the matrix created with above function.
##
## It first checks if inverse is already calculated, if so it returns 
## the cached inverse, otherwise it calculates the inverse and caches
## the inverse via the "setinverse" function
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

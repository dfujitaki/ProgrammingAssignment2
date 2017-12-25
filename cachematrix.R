## Cache the inverse matrix
## If the inverse matrix exists, pull it from the cache, otherwise recalculate the inverse

## First function creates a list of cached matrix objects
## Second function checks if the inverse exists in the cache and either returns that matrix
## (assuming the matrix hasn't changed)
## or recalculates the inverse
makeCacheMatrix <- function(x = matrix()) {

     m <- NULL

     set <- function(y) {

          x <<- y

          m <<- NULL

     }

     get <- function() x

     setmatrix <- function(matrix) m <<- matrix

     getmatrix <- function() m

     list(set = set, get = get,

          setmatrix = setmatrix,

          getmatrix = getmatrix)

}

 

cacheSolve <- function(x, ...) {

     m <- x$getmatrix()

     if(!is.null(m)) {

          message("getting cached data")

          return(m)

     }

     data <- x$get()

     m <- solve(data, ...)

     x$setmatrix(m)

     m

}

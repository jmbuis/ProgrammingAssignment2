## two functions creating a special object that
## stores a numeric matrix and cache's its inverse.


makeCacheMatrix <- function(x = matrix()) {
   ## creates a special "matrix" with functions to
   # 1.set the value of the matrix
   # 2.get the value of the matrix
   # 3.set the value of the inverse
   # 4.get the value of the inverse

   inv <- NULL
   set <- function(y) {
         x <<- y
         inv <<- NULL
   }
   get <- function() x
   setinv <- function(i) inv <<- i
   getinv <- function () inv
   list(set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}


cacheSolve <- function(x, ...) {
   ## Retrieves the cached inverse of x
   ## if not yet cached, calculates the inverse and caches it
   ##

   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setinv(inv)
   inv
}

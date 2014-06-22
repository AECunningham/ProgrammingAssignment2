## These two functions combined will work out the inverse of a matrix, unless it has been worked out already,
## in which case they return the existing inverse from the cache (thus saving processing time)

## Creates a special 'Matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set=set, get=get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the 'Matrix' object created by makeCacheMatrix, or retrieves it if computed already.
## E.g. if z is the original matrix, let zz <- makeCacheMatrix(z), then run cacheSolve(zz)

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
   }
   data <- x$get()
   inv <- solve(data,...)
   x$setinv(inv)
   inv
}





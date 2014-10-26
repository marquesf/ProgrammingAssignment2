## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a cache for a matrix, if the cache does not yet exist
## Or creates a cache and saves it

makeCacheMatrix <- function(mat = numeric()) {
   inv <- NULL
   set <- function(newMat) {
      mat <<- newMat
      m <<- NULL
   }   
   get <- function() mat
   
   setinv <- function(i) inv <<- i
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}



## Write a short comment describing this function
## This function calculates the inverse of a matrix
cacheSolve <- function(mat, ...) {
   i <- mat$getinv()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- mat$get()
   
   # The actual inversion of the matrix
   i <- solve(data, ...)
   
   # Update the cache
   mat$setinv(i)
   i
}


## This function creates a special"matrix" object that cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
mtrx <-NULL
# set the value of the vector
set<-function(y) {
  x<<-y
  mtrx<<-NULL
}
# get the value of the vector
get <-function() x
# Set the Value of the Inverse Matrix
setinvmatrix <- function(solve) mtrx <<- solve
# Get the Value of the Inverse Matrix
getinvmatrix <- function() mtrx
list(set = set, get = get,
     setinvmatrix = setinvmatrix,
     getinvmatrix = getinvmatrix)
}
##
## The cacheSolve function computes the inverse matrix of the special "vector" 
# created with the makeCacheMatrix function. However, it first checks to see if 
# the inverse matrix has already been computed. If so, it gets the 
# inverse matrix from the cache and skips the computation. 
# Otherwise, it computes the inverse matrix of the data and sets the value of 
# the inverse matrix in the cache via the setinvmatrix function.

cacheSolve <- function(x = matrix(), ...) {
       mtrx <-x$getinvmatrix()
# the following returns cached value when available
       if(!is.null(mtrx)) {
         message("getting cached data")
         return(mtrx)
       }
# the following computes inverse matrix when cached value is not available
       data <- x$get()
       mtrx <- solve(data, ...)
# the following sets the value in the cache
       x$setinvmatrix(mtrx)
       mtrx
}





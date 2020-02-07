## Matrix inversion is usually a costly computation and there maybe some benefit to cachin the iversie of a matrix
## rather than compute it repeatedly. The following pair of fuctions is to cache the inverse of a martrix.

## This function is going to create a special "matrix", which is a list containing a function to 
##   1. set the value of the inverse
##   2. get the value of the inverse
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
             i <- NULL
             set <- function(y) {
               x <<- y
               i <<- NULL
             }
             get <- function() x
             setinverse <- function(inverse) i <<- inverse
             getinverse <- function() i
             list(set = set, get = get,
                  setinverse = setinverse,
                  getinverse = getinverse)
}



## The following function caculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
             message("getting cached data")
             return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setinverse(i)
    i
}


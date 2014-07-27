

## This function takes a matrix and it stores it in X
#

makeCacheMatrix <- function(x = matrix()) {
 #initialize to null

  i <- NULL
  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }

  get <-function() x
  setinverse <- function(z) i <<- z
  getinverse <- function() i
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Write a short comment describing this function
#The following function returns an inverted matrix returned by makeCacheMatrix function
#it checks to see if the matrix has been solve and if has,  
#then it returns the matrix from cache without doing any further work. 
#if matrix has not been computed then it calculates the inverse of the matrix
#and it returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## This file contains two functions which help calculate the inverse of a matrix
## the result is cached using makeCacheMatrix function. In case it is already
## calculated the inverse is returned. Otherwise it is calculated and cached

## Contains four functions used to get and set the matrix and the inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Used to calculate the inverse of a matrix if it is not already cached and
## cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

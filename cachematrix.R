##  Computing a matrix inverse can be time consuming especially for big data .The idea here is: if 
##the  matrix does not change ( and supposed to be inversible) , to write functions that can  store 
##  (cache)   the first computation of the matrix inverse , to be called later.This will be done by two 
##   functions ,
##   makeCacheMatrix() and cacheSolve().
##   makeCacheMatrix() creates a new object cacheMatrix that has 2 pieces of imternal data (matrix x 
##   and the cached value) and 4 methodes ( 2 gets and 2 sets).
##  cacheSolve() use output from makeCacheMatrix() ( x,get(),getInverse(), and decide either to ##calculate
## the the inverse or get it from cache.   
makeCacheMatrix <- function(x = matrix()) {
  cachedMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) cachedMatrix <<- Inverse
  getInverse <- function() cachedMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  cachedMatrix <- x$getInverse()
  if(!is.null(cachedMatrix)) {
    message("getting cached data")
    return(cachedMatrix)
  }
  data <- x$get()
  cachedMatrix <- solve(data)
  x$setInverse(cachedMatrix)
  cachedMatrix
}


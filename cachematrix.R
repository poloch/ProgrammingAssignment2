## Put comments here that give an overall description of what your
## functions do

## Builds a CacheMatrix that associates a matrix with a cache for its inverse
## It contains 4 functions:
## get / set to get and set the actual matrix
## getInv / setInv to get and set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInv <- function() inv
  setInv <- function(m) inv <<- m
  list(get=get, set=set, getInv=getInv, setInv=setInv)
}


## Performs the same operation that function solve() over a cacheMatrix.
## Parameter x must be the result of makeCacheMatrix,
##  additionnal parameters will be passed to the solve() method.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    print("getting inverse matrix from the cache")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setInv(inv)
  print("cached inverse matrix")
  inv
}

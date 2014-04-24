## This function creates a "matrix"that is a list.
## containing a function that:
## 1.  sets the value of the matrix,
## 2.  gets the value of the matrix,
## 3.  sets the value of the inverse, and
## 4.  gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # to get the value of the matrix
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  # to get the inverse
  getinv <- function() inv
  # returns a list of all the above
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function computes the inverse of the special "matrix"
## created using makeCacheMatrix.  
## First, the function checks whether the inverse has
## already been calculated.  
## If true, the inverse is retrieved from the cache.
## If false, the inverse of the matrix is calculated and set
## in the cache.

cacheSolve <- function(x, ...) {
  # a check for whether the inverse is cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # not cached, so we get the matrix into data
  data <- x$get()
  inv <- solve(data, ...)
  # then cache the inverse
  x$setinv(inv)
  # and return it as well
  inv
}

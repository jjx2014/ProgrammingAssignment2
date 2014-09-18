## This is a function to get inverse of a matrix
## functions do: "makeCacheMatrix" , "cacheSolve"
            

## Since ginv() is used here, need install and load the library MASS.

library(MASS)

## "makeCacheMatrix" will get the original matrix, set the initial
## inverse to NULL. It will call a function to inverse the matrix.
## It returns a list with getm() -- the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(mtx) m <<- mtx
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
  
}

## "cacheSolve" will cache the inversed data if solved
##              If there were cached data, it would return the
##              cached, or it will compute the inverse and return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setm(m)
  m
  
}

# examples
aa <- makeCacheMatrix(matrix(12:20, 3, 3))
aa$getm()
cacheSolve(aa)
aa$getm()
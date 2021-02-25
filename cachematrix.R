## Put comments here that give an overall description of what your
## functions do
## The functions below will create a special "matrix" object that will be 
## able to cache its inverse, and then compute the inverse of the 
## matrix object.

## Write a short comment describing this function
## The function below will create a special "matrix" object that will cache 
## its inverse. Special Matrix = 'm'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {m <<- inverse}
  getInverse <- function() {m}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function:
## This function will compute the inverse of the matrix returned 
## from the "makeCacheMatrix" function in the previous block of code. 
## If there are no changes in the matrix, then the "cachesSolve" function
## should retrieve the inverse from the cache. Inverse = 'mat_inv'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat_inv <- x$get()
  m <- solve(mat_inv,...)
  x$setInverse(m)
  m
}

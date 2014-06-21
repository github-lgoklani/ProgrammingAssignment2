## Put comments here that give an overall description of what your
## functions do

## Initialize and cache the matrix to retrieve in cacheSolve function
## usage example: 
## mat <- makeCacheMatrix()
## mat$set(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will retrieve inverse from cache if available
## otherwise it will do step to calculate inverse and save it in cache
## usage example:
## cacheSolve(mat)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
        
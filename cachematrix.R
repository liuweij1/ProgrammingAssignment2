## Caching the Inverse of a Matrix

## function that creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs<-NULL
  set<-function(y){
    x<<-y
    invrs<<-NULL
  }
  get<-function()x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}

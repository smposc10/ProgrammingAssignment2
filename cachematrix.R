## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: This is very similar to the
## example functions given on the assignment

makeCacheMatrix <- function(x= matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<-y
    inverse.matrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function below creates an inverse of the matrix in subject using the solve function. In this
## case, it will be the matrix returned by the makeCacheMatrix function. Looks like the mean function example except
## it is using the example.

cacheSolve <- function(x, ...) {
  invm <- x$getinverse()
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinverse(invm)
  invm
}
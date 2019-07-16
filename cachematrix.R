
## These functions create a special object that can hold a matrix and its inverse.
## If the inverse is already stored in the object, it will skip the calculation and return the inverse;
## If the inverse is not availabe, it will calculate the inverse and store it in the object for future use.

## makeCacheMatrix
## Create a special object that can hold matrix and its inverse
## set: set the matrix
## get: get the matrix
## setInverse: set or cache the inverse of the matrix
## getInverse: get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  #variable to store the cached result
  cachedInverse <- NULL
  
  #function, reset the x matrix with new y matrix -- set a matrix, no inverse
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  #function, return the content of the matrix (x)
  get <- function() x
  
  #function, store/cache inverse in the variable cachedInverse
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  #function, return the cached inverse
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
## solve the inverse of a matrix
## check if the inverse has been calculated previously
## yes: extract it from the cache
## no: calculate the inverse and store it for future usage
cacheSolve <- function(x, ...) {
  #extract the cached inverse for the "x" matrix in the special object created by makeCacheMatrix()
  inv <- x$getInverse()
  
  #if it was calculated previously, return the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if there is no cached inverse, get the Matrix
  data <- x$get()
  
  #solve the inverse of the Matrix 
  inv <- solve(data, ...)
  
  #store the inverse in the wrapper object (created by makeCacheMatrix) of the Matrix 
  x$setInverse(inv)
  
  #return the inverse
  inv
}

## run the test
specialMtrx <- makeCacheMatrix()
specialMtrx$get()
specialMtrx$set(matrix(1:4,2,2))
specialMtrx$get()
specialMtrx$getInverse()
cacheSolve(specialMtrx)
cacheSolve(specialMtrx)
